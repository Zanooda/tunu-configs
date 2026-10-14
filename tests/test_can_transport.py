"""SocketCAN receive filtering without hardware."""
import importlib.util
from pathlib import Path
import struct
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, call, patch

spec = importlib.util.spec_from_file_location(
    "tunu_flash_transport", Path(__file__).resolve().parents[1] / "tunu-flash.py")
flash = importlib.util.module_from_spec(spec)
spec.loader.exec_module(flash)


class ReceiveFilterTests(unittest.TestCase):
    def make_transport(self, sender=253):
        sock = Mock()
        with patch.object(flash.socket, "socket", return_value=sock):
            transport = flash.VescCan("can0", 70, sender, 1.0)
        return transport, sock

    def test_filter_is_installed_before_binding(self):
        transport, sock = self.make_transport()
        self.assertEqual(sock.method_calls, [
            call.setsockopt(flash.socket.SOL_CAN_RAW, flash.socket.CAN_RAW_FILTER,
                            struct.pack("=II", 0x800000FD, 0xC00000FF)),
            call.bind(("can0",)),
        ])
        transport.close()
        sock.close.assert_called_once_with()

    def test_filter_accepts_only_addressed_extended_data(self):
        for sender in (251, 253):
            with self.subTest(sender=sender):
                _, sock = self.make_transport(sender)
                wanted, mask = struct.unpack("=II", sock.setsockopt.call_args.args[2])
                matches = lambda frame_id: frame_id & mask == wanted & mask
                for packet_type in (5, 6, 7, 8):
                    self.assertTrue(matches(0x80000000 | packet_type << 8 | sender))
                for frame_id in (0x80000946, 0x80003946, 0x800008FC,
                                 0x7E0, sender, 0xC0000000 | sender):
                    self.assertFalse(matches(frame_id), hex(frame_id))


class ReplPacingTests(unittest.TestCase):
    def test_consecutive_commands_respect_firmware_rate_limit(self):
        clock = [10.0]
        sent = []
        transport = SimpleNamespace(
            send_command=lambda *a, **kw: sent.append(clock[0]),
            recv_reply=lambda **kw: None,
        )

        def sleep(delay):
            clock[0] += delay

        with patch.object(flash.time, "monotonic", side_effect=lambda: clock[0]), \
                patch.object(flash.time, "sleep", side_effect=sleep) as sleeper:
            for elapsed in (0, 0.1, 0.7, 0.3):
                clock[0] += elapsed
                self.assertEqual(flash.cmd_repl(transport, "(+ 1 2)", collect=0), [])
        self.assertEqual(sleeper.call_count, 2)
        for first, second in zip(sent, sent[1:]):
            self.assertGreaterEqual(second - first, 0.6 - 1e-9)
        self.assertEqual(sent[0], 10.0)


if __name__ == "__main__":
    unittest.main()
