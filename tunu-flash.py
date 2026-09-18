#!/usr/bin/env python3
"""
tunu-flash.py - upload LispBM scripts to and run REPL commands on the tunu
VESC motor controller over CAN, straight from the scooter's MDB. No BLE needed.

The tunu ECU is a VESC (firmware 6.06, hardware "TUNU 606") sitting on the
scooter CAN bus at 125 kbit/s with node id 70 (0x46). This script speaks the
VESC CAN transport (raw SocketCAN, python3 stdlib only, no pip deps) so it runs
directly on the MDB.

The VESC only answers while engine power is on. Every subcommand except
`engine` powers the engine on first, waits ~6 s for the VESC to appear, and
turns it back off afterwards (even on failure). Use --no-engine to skip that
if power is already on, or --keep-engine to leave it on. The vehicle stays
locked and parked throughout; this never unlocks.

Usage (copy to the MDB, e.g. scp to deep-blue:/data/, then run there):

    tunu-flash.py version                     query fw version / hw name
    tunu-flash.py repl "(get-vin)"            run one REPL expression, print reply
    tunu-flash.py stats                       lisp engine stats + global bindings
    tunu-flash.py read > current.lisp         read back the installed script
    tunu-flash.py write app.lisp              erase + chunked write + verify + run
    tunu-flash.py output off [seconds]        disable app throttle output (default 120 s)
    tunu-flash.py output on                   re-enable app throttle output
    tunu-flash.py engine on|off               just toggle engine power via lsc

Common flags:
    --node N        target node id (default 70 / 0x46)
    --sender N      our sender id on the bus (default 253 / 0xFD, arbitrary)
    --iface NAME    CAN interface (default can0)
    --timeout SEC   per-reply timeout (default 1.0)
    --no-engine     do not touch engine power (assume it is already on)
    --keep-engine   leave the engine on afterwards (default restores prior state)

The `write` path first disables the app throttle output for 120 s via the
REPL extension (app-disable-output), because the appconf runs app_to_use=5
(ADC+UART): the physical throttle is live whenever the ECU is powered, and a
twist during an upload would move the motor. Only then does it stop lisp
(SET_RUNNING 0), erase, write the image in CRC-checked chunks, read it back to
verify, and restart lisp (SET_RUNNING 1), finishing with (app-disable-output 0)
to re-enable output. If that final REPL fails, output re-enables on its own
when the 120 s window expires. The whole write aborts if the initial
disable-output call does not confirm, or if the target does not answer a
version query. The `output` subcommand exposes the same guard manually;
app_disable_output lives in app.c, independent of the lisp engine, so it works
even with no script installed.
"""

import argparse
import errno
import os
import socket
import struct
import subprocess
import sys
import time

# --- VESC CAN packet types (datatypes.h CAN_PACKET_*) ---
CAN_PACKET_FILL_RX_BUFFER = 5
CAN_PACKET_FILL_RX_BUFFER_LONG = 6
CAN_PACKET_PROCESS_RX_BUFFER = 7
CAN_PACKET_PROCESS_SHORT_BUFFER = 8

# --- VESC COMM command ids (datatypes.h COMM_*) ---
COMM_FW_VERSION = 0
COMM_LISP_READ_CODE = 130
COMM_LISP_WRITE_CODE = 131
COMM_LISP_ERASE_CODE = 132
COMM_LISP_SET_RUNNING = 133
COMM_LISP_GET_STATS = 134
COMM_LISP_PRINT = 135
COMM_LISP_REPL_CMD = 138

# Transport buffer cap on the target (RX_BUFFER_SIZE = PACKET_MAX_PL_LEN = 512).
# A single buffered command payload (including the [sender, flag] prefix on the
# wire, but that lives in separate fill frames) must fit in this many bytes.
RX_BUFFER_SIZE = 512

CAN_EFF_FLAG = 0x80000000
CAN_FRAME_FMT = "=IB3x8s"  # can_id, can_dlc, 3 pad, 8 data

# CRC16 CCITT, poly 0x1021, init 0x0000, no reflection (util/crc.c crc16()).
_CRC16_TAB = None


def crc16(data):
    global _CRC16_TAB
    if _CRC16_TAB is None:
        tab = []
        for i in range(256):
            c = i << 8
            for _ in range(8):
                c = ((c << 1) ^ 0x1021) if (c & 0x8000) else (c << 1)
                c &= 0xFFFF
            tab.append(c)
        _CRC16_TAB = tab
    cksum = 0
    for b in data:
        cksum = (_CRC16_TAB[((cksum >> 8) ^ b) & 0xFF] ^ ((cksum << 8) & 0xFFFF)) & 0xFFFF
    return cksum


def be32(n):
    return struct.pack(">I", n & 0xFFFFFFFF)


def be16(n):
    return struct.pack(">H", n & 0xFFFF)


class VescCan:
    def __init__(self, iface, node, sender, timeout):
        self.node = node
        self.sender = sender
        self.timeout = timeout
        self.sock = socket.socket(socket.AF_CAN, socket.SOCK_RAW, socket.CAN_RAW)
        self.sock.bind((iface,))

    def close(self):
        self.sock.close()

    # -- raw frame io --
    def _tx(self, eid, data):
        assert len(data) <= 8
        frame = struct.pack(
            CAN_FRAME_FMT, (eid & 0x1FFFFFFF) | CAN_EFF_FLAG, len(data),
            bytes(data).ljust(8, b"\x00"))
        # ENOBUFS = TX queue full, typically because the ECU is busy erasing or
        # programming flash and not ACKing quickly. That is transient during a
        # multi-chunk write, so keep retrying for a few seconds rather than
        # aborting with the image half written.
        for attempt in range(600):
            try:
                self.sock.send(frame)
                break
            except OSError as error:
                if error.errno != errno.ENOBUFS or attempt == 599:
                    raise
                time.sleep(0.01)
        time.sleep(0.001)

    def _rx(self, deadline):
        """Read one extended frame addressed to our sender id. Returns
        (ptype, data) or None on timeout. Filters non-EFF and other targets."""
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return None
            self.sock.settimeout(remaining)
            try:
                raw = self.sock.recv(16)
            except socket.timeout:
                return None
            can_id, dlc, payload = struct.unpack(CAN_FRAME_FMT, raw)
            if not (can_id & CAN_EFF_FLAG):
                continue
            eid = can_id & 0x1FFFFFFF
            if (eid & 0xFF) != self.sender:
                continue
            ptype = (eid >> 8) & 0xFF
            yield_data = payload[:dlc]
            return (ptype, yield_data)

    # -- send a VESC command (comm_can_send_buffer) --
    def send_command(self, payload, send_flag=0):
        """payload = [COMM_ID, ...args]. Fragments per comm_can_send_buffer."""
        payload = bytes(payload)
        n = len(payload)
        if n <= 6:
            frame = bytes([self.sender, send_flag]) + payload
            self._tx(self.node | (CAN_PACKET_PROCESS_SHORT_BUFFER << 8), frame)
            return
        if n > RX_BUFFER_SIZE:
            raise ValueError("command payload %d exceeds RX_BUFFER_SIZE %d" % (n, RX_BUFFER_SIZE))
        # Fill using the short-offset variant while offset fits one byte,
        # then the long-offset variant, exactly like the firmware sender.
        end_a = 0
        i = 0
        while i < n:
            if i > 255:
                break
            end_a = i + 7
            chunk = payload[i:i + 7]
            self._tx(self.node | (CAN_PACKET_FILL_RX_BUFFER << 8),
                     bytes([i]) + chunk)
            i += 7
        i = end_a
        while i < n:
            chunk = payload[i:i + 6]
            self._tx(self.node | (CAN_PACKET_FILL_RX_BUFFER_LONG << 8),
                     bytes([(i >> 8) & 0xFF, i & 0xFF]) + chunk)
            i += 6
        crc = crc16(payload)
        tail = bytes([self.sender, send_flag]) + be16(n) + be16(crc)
        self._tx(self.node | (CAN_PACKET_PROCESS_RX_BUFFER << 8), tail)

    # -- receive one reassembled VESC reply payload --
    def recv_reply(self, timeout=None):
        """Collect frames until a short buffer or a process-rx-buffer completes
        a reply. Returns the reply payload bytes (starting with its COMM id), or
        None on timeout. Tolerates out-of-order fill frames."""
        if timeout is None:
            timeout = self.timeout
        deadline = time.monotonic() + timeout
        fills = {}  # offset -> bytes
        while True:
            got = self._rx(deadline)
            if got is None:
                return None
            ptype, data = got
            if ptype == CAN_PACKET_PROCESS_SHORT_BUFFER:
                # [last_id, send_flag, ...payload]
                return bytes(data[2:])
            elif ptype == CAN_PACKET_FILL_RX_BUFFER:
                off = data[0]
                fills[off] = bytes(data[1:])
            elif ptype == CAN_PACKET_FILL_RX_BUFFER_LONG:
                off = (data[0] << 8) | data[1]
                fills[off] = bytes(data[2:])
            elif ptype == CAN_PACKET_PROCESS_RX_BUFFER:
                # [last_id, send_flag, len_hi, len_lo, crc_hi, crc_lo]
                length = (data[2] << 8) | data[3]
                crc_want = (data[4] << 8) | data[5]
                buf = bytearray(length)
                filled = bytearray(length)
                for off, chunk in fills.items():
                    for k, byte in enumerate(chunk):
                        if off + k < length:
                            buf[off + k] = byte
                            filled[off + k] = 1
                if not all(filled):
                    # missing fragment; keep waiting for a resend/retry
                    continue
                if crc16(buf) != crc_want:
                    raise IOError("reply CRC mismatch (got 0x%04X want 0x%04X)"
                                  % (crc16(buf), crc_want))
                return bytes(buf)
            # other ptypes (status broadcasts etc.) are ignored

    def query(self, payload, send_flag=0, expect_id=None, retries=3, timeout=None):
        """Send a command and wait for a reply whose first byte is expect_id
        (defaults to the command id). Retries on timeout."""
        if expect_id is None:
            expect_id = payload[0]
        last_err = None
        for _ in range(retries):
            self.send_command(payload, send_flag)
            try:
                reply = self.recv_reply(timeout)
            except IOError as e:
                last_err = e
                continue
            if reply is None:
                continue
            if reply and reply[0] == expect_id:
                return reply
            # a stray reply of another type; keep listening briefly
            deadline = time.monotonic() + (timeout or self.timeout)
            while reply is not None and reply[0] != expect_id and time.monotonic() < deadline:
                reply = self.recv_reply(0.2)
                if reply and reply[0] == expect_id:
                    return reply
        if last_err:
            raise last_err
        return None


# --- float32_auto decode (util/buffer.c buffer_get_float32_auto) ---
def get_float32_auto(buf, idx):
    res = struct.unpack_from(">I", buf, idx)[0]
    idx += 4
    e = (res >> 23) & 0xFF
    sig_i = res & 0x7FFFFF
    neg = bool(res & (1 << 31))
    sig = 0.0
    if e != 0 or sig_i != 0:
        sig = sig_i / (8388608.0 * 2.0) + 0.5
        e -= 126
    if neg:
        sig = -sig
    return (sig * (2.0 ** e), idx)


def get_float16(buf, idx, scale):
    val = struct.unpack_from(">h", buf, idx)[0]
    return (val / scale, idx + 2)


# --- high level operations ---
def cmd_version(v):
    reply = v.query([COMM_FW_VERSION], expect_id=COMM_FW_VERSION)
    if reply is None:
        return None
    idx = 1
    major = reply[idx]; idx += 1
    minor = reply[idx]; idx += 1
    end = reply.index(0, idx)
    hw_name = reply[idx:end].decode("ascii", "replace")
    idx = end + 1
    uuid = reply[idx:idx + 12]
    return {
        "major": major, "minor": minor, "hw_name": hw_name,
        "uuid": uuid.hex(),
    }


def cmd_repl(v, expr, collect=2.0):
    """Send a REPL expression and gather the LISP_PRINT output it produces."""
    payload = bytes([COMM_LISP_REPL_CMD]) + expr.encode("utf-8") + b"\x00"
    v.send_command(payload, send_flag=0)
    lines = []
    deadline = time.monotonic() + collect
    while time.monotonic() < deadline:
        reply = v.recv_reply(timeout=min(1.0, max(0.05, deadline - time.monotonic())))
        if reply is None:
            continue
        if reply[0] == COMM_LISP_PRINT:
            text = reply[1:].split(b"\x00", 1)[0].decode("utf-8", "replace")
            lines.append(text)
            # a "> " result line usually ends the exchange; give a short grace
            deadline = min(deadline, time.monotonic() + 0.4)
    return lines


def cmd_stats(v):
    reply = v.query([COMM_LISP_GET_STATS, 1], expect_id=COMM_LISP_GET_STATS, timeout=2.0)
    if reply is None:
        return None
    idx = 1
    cpu, idx = get_float16(reply, idx, 1e2)
    heap, idx = get_float16(reply, idx, 1e2)
    mem, idx = get_float16(reply, idx, 1e2)
    _stack, idx = get_float16(reply, idx, 1e2)
    idx += 1  # unused result byte '\0'
    bindings = []
    while idx < len(reply):
        end = reply.find(b"\x00", idx)
        if end < 0 or end + 4 >= len(reply):
            break
        name = reply[idx:end].decode("ascii", "replace")
        idx = end + 1
        val, idx = get_float32_auto(reply, idx)
        bindings.append((name, val))
    return {"cpu": cpu, "heap": heap, "mem": mem, "bindings": bindings}


def cmd_read(v):
    """Read the installed lisp code blob (source + null + import table)."""
    # first request to learn total length
    reply = v.query([COMM_LISP_READ_CODE] + list(be32(1)) + list(be32(0)),
                    expect_id=COMM_LISP_READ_CODE, timeout=2.0)
    if reply is None:
        raise IOError("no reply to READ_CODE")
    total = struct.unpack_from(">i", reply, 1)[0]
    if total <= 0:
        return b""
    out = bytearray(total)
    ofs = 0
    chunk = 400
    while ofs < total:
        want = min(chunk, total - ofs)
        reply = v.query(
            [COMM_LISP_READ_CODE] + list(be32(want)) + list(be32(ofs)),
            expect_id=COMM_LISP_READ_CODE, timeout=2.0)
        if reply is None:
            raise IOError("no reply to READ_CODE at offset %d" % ofs)
        rlen = struct.unpack_from(">i", reply, 1)[0]
        rofs = struct.unpack_from(">i", reply, 5)[0]
        data = reply[9:]
        if rofs != ofs:
            raise IOError("READ_CODE offset mismatch: asked %d got %d" % (ofs, rofs))
        out[ofs:ofs + len(data)] = data
        ofs += len(data)
        if len(data) == 0:
            raise IOError("READ_CODE returned empty chunk at %d" % ofs)
        total = rlen  # trust the reported total
    return bytes(out)


def build_image(source_bytes):
    """Wrap raw lisp source as the flash image VESC Tool writes:
    [len_u32][crc_u16][flags_u16][blob], blob = source + NUL + u16 import
    count (0; import tables are not supported here), crc = crc16(flags_bytes
    + blob). Verified byte-identical against an image VESC Tool installed."""
    blob = source_bytes + b"\x00" + b"\x00\x00"
    flags = b"\x00\x00"
    crc = crc16(flags + blob)
    header = be32(len(blob)) + be16(crc) + flags
    return header + blob, blob


def cmd_output_disable(v, ms):
    """Call (app-disable-output ms) via the REPL. ms=0 re-enables output
    immediately, ms>0 disables it for that long. Returns True when the
    extension confirmed with t."""
    lines = cmd_repl(v, "(app-disable-output %d)" % ms, collect=2.0)
    return any(line.strip() == "> t" for line in lines)


def cmd_set_running(v, running):
    reply = v.query([COMM_LISP_SET_RUNNING, 1 if running else 0],
                    expect_id=COMM_LISP_SET_RUNNING, timeout=3.0)
    if reply is None:
        raise IOError("no reply to SET_RUNNING")
    return reply[1] == 1


def cmd_erase(v):
    reply = v.query([COMM_LISP_ERASE_CODE], expect_id=COMM_LISP_ERASE_CODE, timeout=5.0)
    if reply is None:
        raise IOError("no reply to ERASE_CODE")
    return reply[1] == 1


def cmd_write(v, source_bytes, progress=True):
    image, blob = build_image(source_bytes)
    # WRITE_CODE payload = [131, offset_u32, ...chunk]; keep well under 512.
    chunk = 384
    ofs = 0
    total = len(image)
    while ofs < total:
        part = image[ofs:ofs + chunk]
        payload = bytes([COMM_LISP_WRITE_CODE]) + be32(ofs) + part
        # Mid-image the ECU can stall while it erases and programs flash, so a
        # missing reply is not fatal: the ack carries the offset, which makes
        # resending the same chunk idempotent. Retry before giving up.
        reply = None
        for attempt in range(6):
            try:
                reply = v.query(payload, expect_id=COMM_LISP_WRITE_CODE, timeout=5.0)
            except OSError as error:
                reply = None
                sys.stderr.write("\n  tx stalled at offset %d (%s)\n" % (ofs, error))
            if reply is not None:
                break
            sys.stderr.write("\n  no reply at offset %d, retry %d/6\n" % (ofs, attempt + 1))
            sys.stderr.flush()
            time.sleep(0.5)
        if reply is None:
            raise IOError("no reply to WRITE_CODE at offset %d" % ofs)
        if reply[1] != 1:
            raise IOError("WRITE_CODE reported failure at offset %d" % ofs)
        acked = struct.unpack_from(">I", reply, 2)[0]
        if acked != ofs:
            raise IOError("WRITE_CODE offset ack mismatch: %d vs %d" % (acked, ofs))
        ofs += len(part)
        if progress:
            sys.stderr.write("\r  wrote %d/%d bytes" % (ofs, total))
            sys.stderr.flush()
    if progress:
        sys.stderr.write("\n")
    return blob


# --- engine power management via lsc ---
def engine_set(state):
    # lsc prints "✓ Engine power: on" to stdout. Keep it off stdout: `read` and
    # `write` use stdout as the data channel, and mixing the two silently
    # corrupts the output (a readback then differs from the file it came from).
    res = subprocess.run(["lsc", "engine", state], check=False, timeout=15,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    if res.stdout:
        sys.stderr.write(res.stdout)


def main():
    ap = argparse.ArgumentParser(description="tunu VESC CAN flasher / REPL")
    ap.add_argument("--node", type=lambda x: int(x, 0), default=70)
    ap.add_argument("--sender", type=lambda x: int(x, 0), default=0xFD)
    ap.add_argument("--iface", default="can0")
    ap.add_argument("--timeout", type=float, default=1.0)
    ap.add_argument("--no-engine", action="store_true",
                    help="do not toggle engine power (assume already on)")
    ap.add_argument("--keep-engine", action="store_true",
                    help="leave engine on after the command")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("version")
    p = sub.add_parser("repl"); p.add_argument("expr")
    sub.add_parser("stats")
    sub.add_parser("read")
    p = sub.add_parser("write"); p.add_argument("file")
    p = sub.add_parser("output")
    p.add_argument("state", choices=["on", "off"])
    p.add_argument("seconds", nargs="?", type=int, default=120)
    p = sub.add_parser("engine"); p.add_argument("state", choices=["on", "off"])
    args = ap.parse_args()

    # `engine` subcommand is a thin wrapper, no CAN.
    if args.cmd == "engine":
        engine_set(args.state)
        return 0

    manage_engine = not args.no_engine
    if manage_engine:
        engine_set("on")
        time.sleep(6)

    v = VescCan(args.iface, args.node, args.sender, args.timeout)
    rc = 0
    try:
        if args.cmd == "version":
            info = cmd_version(v)
            if info is None:
                sys.stderr.write("no response from node 0x%02X\n" % args.node)
                rc = 1
            else:
                print("fw %d.%02d  hw '%s'  uuid %s"
                      % (info["major"], info["minor"], info["hw_name"], info["uuid"]))

        elif args.cmd == "repl":
            for line in cmd_repl(v, args.expr):
                print(line)

        elif args.cmd == "stats":
            s = cmd_stats(v)
            if s is None:
                sys.stderr.write("no stats (lisp not running?)\n")
                rc = 1
            else:
                print("cpu %.2f%%  heap %.2f%%  mem %.2f%%" % (s["cpu"], s["heap"], s["mem"]))
                for name, val in s["bindings"]:
                    print("  %-24s %g" % (name, val))

        elif args.cmd == "output":
            ms = 0 if args.state == "on" else args.seconds * 1000
            if cmd_output_disable(v, ms):
                if ms == 0:
                    print("app output re-enabled")
                else:
                    print("app output disabled for %d s" % args.seconds)
            else:
                sys.stderr.write("app-disable-output did not confirm\n")
                rc = 1

        elif args.cmd == "read":
            blob = cmd_read(v)
            source = blob.split(b"\x00", 1)[0]
            sys.stdout.buffer.write(source)
            if source and not source.endswith(b"\n"):
                sys.stdout.buffer.write(b"\n")

        elif args.cmd == "write":
            # Refuse to write unless the target answers version first.
            info = cmd_version(v)
            if info is None:
                sys.stderr.write("refusing to write: node 0x%02X did not answer version\n"
                                 % args.node)
                rc = 1
            else:
                sys.stderr.write("target: fw %d.%02d hw '%s'\n"
                                 % (info["major"], info["minor"], info["hw_name"]))
                with open(args.file, "rb") as f:
                    source = f.read()
                # The physical throttle is live whenever the ECU is powered
                # (app_to_use=5). Kill app output before touching anything so
                # a twist during the upload cannot move the motor.
                sys.stderr.write("disabling app output for 120 s...\n")
                if not cmd_output_disable(v, 120000):
                    raise IOError("aborting: (app-disable-output 120000) did not confirm")
                sys.stderr.write("stopping lisp...\n")
                cmd_set_running(v, False)
                sys.stderr.write("erasing...\n")
                if not cmd_erase(v):
                    raise IOError("erase failed")
                sys.stderr.write("writing %d bytes of source...\n" % len(source))
                blob = cmd_write(v, source)
                sys.stderr.write("verifying...\n")
                back = cmd_read(v)
                if back != blob:
                    raise IOError("verify mismatch: read %d bytes, expected %d"
                                  % (len(back), len(blob)))
                sys.stderr.write("restarting lisp...\n")
                cmd_set_running(v, True)
                sys.stderr.write("re-enabling app output...\n")
                time.sleep(1.0)  # REPL rate limit, and let the new script boot
                if cmd_output_disable(v, 0):
                    sys.stderr.write("done.\n")
                else:
                    sys.stderr.write("done, but re-enable did not confirm; "
                                     "output returns when the 120 s window expires\n")
    finally:
        v.close()
        if manage_engine and not args.keep_engine:
            engine_set("off")
    return rc


if __name__ == "__main__":
    sys.exit(main())
