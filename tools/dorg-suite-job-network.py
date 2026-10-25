#!/usr/bin/env python3
"""Observe the suite container namespace before admitting source bytes."""

import argparse
import errno
from pathlib import Path
import socket
import sys


class NetworkPolicyError(Exception):
    """The observed namespace does not meet the no-egress policy."""


def _route_state(text, family):
    if text is None:
        return "missing", "fail"
    rows = text.splitlines()
    if family == 4:
        if not rows or not rows[0].split() or rows[0].split()[0] != "Iface":
            return "invalid", "fail"
        rows = rows[1:]
    observed = []
    policy = "pass"
    for row in rows:
        fields = row.split()
        if not fields or len(fields) < (8 if family == 4 else 10):
            return "invalid", "fail"
        try:
            if family == 4:
                # /proc/net/route prints each IPv4 address as little-endian
                # hex; the low byte is its first dotted-decimal octet.
                if len(fields[1]) != 8 or len(fields[7]) != 8:
                    return "invalid", "fail"
                destination = int(fields[1], 16)
                mask = int(fields[7], 16)
                flags = int(fields[3], 16)
                interface = fields[0]
                route = f"{fields[1]}/{fields[7]}"
                is_loopback = (fields[0] == "lo" and
                               destination & 0xFF == 127 and
                               mask & 0xFF == 0xFF)
            else:
                destination = fields[0]
                if len(destination) != 32:
                    return "invalid", "fail"
                int(destination, 16)
                prefix = int(fields[1], 16)
                if not 0 <= prefix <= 128:
                    return "invalid", "fail"
                flags = int(fields[8], 16)
                interface = fields[-1]
                route = f"{fields[0]}/{fields[1]}"
                is_loopback = (fields[-1] == "lo" and
                               destination == "0" * 31 + "1" and
                               prefix == 128)
        except ValueError:
            return "invalid", "fail"
        # RTF_REJECT means the kernel discards this route. Docker's isolated
        # namespace includes an IPv6 reject default via lo.
        if flags & 0x200:
            kind = "reject"
        elif is_loopback:
            kind = "loopback"
        else:
            kind = "egress"
            policy = "fail"
        observed.append(f"{kind}:{interface}:{route}")
    return ";".join(observed) or "none", policy


def _interface_state(interfaces):
    if "lo" not in interfaces:
        return "missing-loopback", "fail"
    observed = []
    policy = "pass"
    for name, details in sorted(interfaces.items()):
        try:
            kind = details["type"]
            state = details["operstate"]
            flags = details["flags"]
            if not kind or not state or not isinstance(flags, int):
                raise ValueError("incomplete interface")
        except (KeyError, TypeError, ValueError):
            return "invalid-interface", "fail"
        observed.append(
            f"{name}|type={kind}|operstate={state}|flags=0x{flags:x}")
        is_up = bool(flags & 0x1)  # Linux IFF_UP, not operstate.
        if (name == "lo" and not is_up) or (name != "lo" and is_up):
            policy = "fail"
    return ";".join(observed), policy


def _connect_result(connector, family, address):
    try:
        code = connector(family, address, 443, 2.0)
    except TimeoutError:
        return "TIMEOUT"
    except OSError as exc:
        code = exc.errno
    if code == 0:
        return "CONNECTED"
    return errno.errorcode.get(code, "UNKNOWN")


def observe(interfaces, routes4, routes6, connector):
    """Return actual namespace and outbound-attempt observations."""
    interface_detail, interface_policy = _interface_state(interfaces)
    route4_detail, route4_policy = _route_state(routes4, 4)
    route6_detail, route6_policy = _route_state(routes6, 6)
    return {
        "network_interfaces": interface_detail,
        "network_interface_policy": interface_policy,
        "network_ipv4_routes": route4_detail,
        "network_ipv4_route_policy": route4_policy,
        "network_ipv6_routes": route6_detail,
        "network_ipv6_route_policy": route6_policy,
        "network_ipv4_connect": _connect_result(
            connector, socket.AF_INET, "192.0.2.1"),
        "network_ipv6_connect": _connect_result(
            connector, socket.AF_INET6, "2001:db8::1"),
    }


def assess(observed):
    """Return the observed mode, or reject incomplete/non-isolated evidence."""
    if observed["network_interface_policy"] != "pass":
        raise NetworkPolicyError("UP non-loopback or missing interface")
    for family in ("ipv4", "ipv6"):
        label = "IPv" + family[-1]
        if observed[f"network_{family}_route_policy"] != "pass":
            raise NetworkPolicyError(f"{label} route permits egress")
        if observed[f"network_{family}_connect"] not in {
            "ENETUNREACH", "EHOSTUNREACH", "EADDRNOTAVAIL"
        }:
            raise NetworkPolicyError(
                f"{label} outbound attempt was not unreachable")
    return "none"


def format_receipt(observed):
    """Render an auditable receipt with the mode derived from observations."""
    try:
        mode = assess(observed)
        result = "pass"
        reason = ""
    except NetworkPolicyError as exc:
        mode = "non-isolated"
        result = "fail"
        reason = str(exc)
    fields = {"network_mode": mode, **observed,
              "network_probe_result": result,
              "network_probe_failure": reason}
    return "".join(f"{key}={value}\n" for key, value in fields.items())


def _connect(family, address, port, timeout):
    with socket.socket(family, socket.SOCK_STREAM) as connection:
        connection.settimeout(timeout)
        return connection.connect_ex((address, port))


def _read_interfaces():
    interfaces = {}
    for path in Path("/sys/class/net").iterdir():
        if not path.is_dir():
            continue
        interfaces[path.name] = {
            "type": (path / "type").read_text().strip(),
            "operstate": (path / "operstate").read_text().strip(),
            "flags": int((path / "flags").read_text().strip(), 16),
        }
    return interfaces


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--receipt", type=Path, required=True)
    args = parser.parse_args()
    try:
        interfaces = _read_interfaces()
        routes4 = Path("/proc/net/route").read_text()
        routes6 = Path("/proc/net/ipv6_route").read_text()
        observed = observe(interfaces, routes4, routes6, _connect)
        receipt = format_receipt(observed)
        args.receipt.write_text(receipt)
        if "network_probe_result=pass\n" not in receipt:
            print(receipt, file=sys.stderr, end="")
            return 2
        return 0
    except (OSError, ValueError) as exc:
        args.receipt.write_text(
            "network_mode=unknown\nnetwork_probe_result=fail\n"
            f"network_probe_failure={type(exc).__name__}\n")
        print(f"network probe evidence unavailable: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main())
