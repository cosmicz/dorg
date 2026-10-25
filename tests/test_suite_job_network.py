"""Controls for the suite container's observed outbound network policy."""

import base64
import errno
import importlib.util
from pathlib import Path
import socket
import unittest


PROBE = Path(__file__).resolve().parents[1] / "tools/dorg-suite-job-network.py"
CAPTURE = Path(__file__).resolve().parent / "fixtures/network/none-linuxkit.txt.b64"
spec = importlib.util.spec_from_file_location("dorg_suite_job_network", PROBE)
network = importlib.util.module_from_spec(spec)
spec.loader.exec_module(network)


class NetworkProbe(unittest.TestCase):
    LOOPBACK = {"lo": {"type": "772", "operstate": "unknown", "flags": 0x9}}
    ROUTES4 = ("Iface Destination Gateway Flags RefCnt Use Metric Mask MTU Window IRTT\n"
               "lo 0000007F 00000000 0001 0 0 0 000000FF 0 0 0\n")
    ROUTES6 = ("0" * 31 + "1" + " 80 " + "0" * 32 + " 00 " + "0" * 32
               + " 00000000 00000000 00000000 00000001 lo\n")

    def observe(self, interfaces=None, routes4=ROUTES4, routes6=ROUTES6,
                connect=None):
        if interfaces is None:
            interfaces = self.LOOPBACK
        if connect is None:
            connect = lambda *_: errno.ENETUNREACH
        return network.observe(interfaces, routes4, routes6, connect)

    def test_loopback_only_attempts_both_non_loopback_families(self):
        calls = []

        def connect(family, address, port, timeout):
            calls.append((family, address, port, timeout))
            return errno.ENETUNREACH

        observed = self.observe(connect=connect)
        self.assertEqual(network.assess(observed), "none")
        self.assertEqual(calls, [(socket.AF_INET, "192.0.2.1", 443, 2.0),
                                 (socket.AF_INET6, "2001:db8::1", 443, 2.0)])
        self.assertIn("network_mode=none", network.format_receipt(observed))
        self.assertIn("network_ipv6_connect=ENETUNREACH",
                      network.format_receipt(observed))

    def test_down_kernel_tunnel_is_allowed_but_up_tunnel_refuses(self):
        down = {**self.LOOPBACK,
                "gre0": {"type": "778", "operstate": "down", "flags": 0x80}}
        self.assertEqual(network.assess(self.observe(interfaces=down)), "none")
        up = {**self.LOOPBACK,
              "gre0": {"type": "778", "operstate": "up", "flags": 0x81}}
        with self.assertRaisesRegex(network.NetworkPolicyError, "interface"):
            network.assess(self.observe(interfaces=up))
        with self.assertRaisesRegex(network.NetworkPolicyError, "interface"):
            network.assess(self.observe(interfaces={}))
        down_loopback = {"lo": {"type": "772", "operstate": "down", "flags": 0x8}}
        with self.assertRaisesRegex(network.NetworkPolicyError, "interface"):
            network.assess(self.observe(interfaces=down_loopback))

    def test_non_loopback_ipv4_route_refuses(self):
        routes = self.ROUTES4 + "eth0 00000000 010011AC 0003 0 0 0 00000000 0 0 0\n"
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv4 route"):
            network.assess(self.observe(routes4=routes))
        default_via_loopback = self.ROUTES4.replace(
            "lo 0000007F 00000000 0001 0 0 0 000000FF",
            "lo 00000000 00000000 0001 0 0 0 00000000")
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv4 route"):
            network.assess(self.observe(routes4=default_via_loopback))
        reject_default = self.ROUTES4.replace(
            "lo 0000007F 00000000 0001 0 0 0 000000FF",
            "lo 00000000 00000000 0200 0 0 0 00000000")
        observed = self.observe(routes4=reject_default)
        self.assertEqual(network.assess(observed), "none")
        self.assertIn("reject:lo:", network.format_receipt(observed))

    def test_non_loopback_ipv6_route_refuses(self):
        routes = self.ROUTES6.replace(" lo\n", " eth0\n")
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv6 route"):
            network.assess(self.observe(routes6=routes))
        default_via_loopback = self.ROUTES6.replace("0" * 31 + "1" + " 80",
                                                    "0" * 32 + " 00")
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv6 route"):
            network.assess(self.observe(routes6=default_via_loopback))

    def test_real_linuxkit_none_capture_passes_and_records_reject_routes(self):
        # Base64 retains the kernel capture's original trailing whitespace.
        text = base64.b64decode(CAPTURE.read_bytes()).decode()
        interface_part, tail = text.split("ROUTE4\n", 1)
        route4, tail = tail.split("ROUTE6\n", 1)
        route6 = tail.split("sha256:", 1)[0]
        interfaces = {}
        for line in interface_part.splitlines():
            fields = line.split()
            if len(fields) == 4 and fields[1].startswith("type="):
                name = fields[0]
                kind = fields[1].removeprefix("type=")
                if kind:  # bonding_masters is a file, not an interface.
                    interfaces[name] = {
                        "type": kind,
                        "operstate": fields[2].removeprefix("operstate="),
                        "flags": int(fields[3].removeprefix("flags="), 16),
                    }
        observed = self.observe(interfaces=interfaces,
                                routes4=route4.replace("\\t", "\t"),
                                routes6=route6)
        self.assertEqual(network.assess(observed), "none")
        receipt = network.format_receipt(observed)
        self.assertIn("network_mode=none", receipt)
        self.assertIn("reject:lo:", receipt)
        self.assertIn("gre0|type=778|operstate=down|flags=0x80", receipt)

        interfaces["gre0"]["flags"] |= 1
        with self.assertRaisesRegex(network.NetworkPolicyError, "interface"):
            network.assess(self.observe(interfaces=interfaces,
                                        routes4=route4.replace("\\t", "\t"),
                                        routes6=route6))
        nonreject = route6.replace("00200200", "00200000", 1).replace(
            "       lo", "       eth0", 1)
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv6 route"):
            network.assess(self.observe(routes4=route4.replace("\\t", "\t"),
                                        routes6=nonreject))

    def test_successful_connect_fails(self):
        observed = self.observe(connect=lambda *_: 0)
        with self.assertRaisesRegex(network.NetworkPolicyError,
                                    "outbound attempt"):
            network.assess(observed)

    def test_connection_refused_fails(self):
        observed = self.observe(connect=lambda *_: errno.ECONNREFUSED)
        with self.assertRaisesRegex(network.NetworkPolicyError,
                                    "outbound attempt"):
            network.assess(observed)

    def test_timeout_fails(self):
        def timeout(*_):
            raise TimeoutError("network probe timed out")

        with self.assertRaisesRegex(network.NetworkPolicyError,
                                    "outbound attempt"):
            network.assess(self.observe(connect=timeout))

    def test_missing_route_evidence_refuses_and_allowed_errnos_pass(self):
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv4 route"):
            network.assess(self.observe(routes4=""))
        with self.assertRaisesRegex(network.NetworkPolicyError, "IPv6 route"):
            network.assess(self.observe(routes6=None))
        for code in (errno.ENETUNREACH, errno.EHOSTUNREACH,
                     errno.EADDRNOTAVAIL):
            with self.subTest(code=code):
                self.assertEqual(network.assess(
                    self.observe(connect=lambda *_: code)), "none")


if __name__ == "__main__":
    unittest.main()
