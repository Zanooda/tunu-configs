"""SocketCAN receive filtering without hardware."""
import importlib.util
from pathlib import Path
import struct
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


if __name__ == "__main__":
    unittest.main()
