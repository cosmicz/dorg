"""Exercise the real host launcher with owned fake Docker processes, no daemon."""

import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[1]
CID = "a" * 64
FOREIGN = "b" * 64
DOCKER = r'''#!/usr/bin/env python3
import json, os, pathlib, sys, time
s = pathlib.Path(os.environ["FAKE_STATE"])
a = sys.argv[1:]
mode = os.environ.get("FAKE_MODE", "running")
cid = "a" * 64
foreign = "b" * 64
with (s / "calls").open("a") as f:
    f.write(json.dumps(a) + "\n")
if a[0] == "ps":
    print(foreign)
    if (s / "container").exists(): print(cid)
elif a[0] == "volume":
    pass
elif a[0] == "run":
    def option(k): return a[a.index(k) + 1] if k in a else None
    art = pathlib.Path(option("-v").removesuffix(":/artifacts"))
    (s / "client").write_text(str(os.getpid()))
    label = option("--label") or ""
    (s / "label").write_text(label.partition("=")[2])
    if mode != "before-create":
        (s / "container").write_text(cid)
        if option("--cidfile") and mode != "no-cidfile":
            pathlib.Path(option("--cidfile")).write_text(
                "invalid" if mode == "invalid-cid" else cid)
    (art / "exit.status").write_text("compile=0\nlint=0\n")
    (art / "raw.log").write_text("retained-before-interruption\n")
    if mode == "green-then-wait":
        # The container already wrote a complete green receipt; the host is
        # interrupted before the client returns.
        (art / "exit.status").write_text(
            "compile=0\nlint=0\ntest=0\noverall=0\nclassification=pass\n")
        (art / "manifest.txt").write_text("artifact_completeness=complete\n")
        (art / "ert-summary.txt").write_text(
            "Ran 1 tests, 1 results as expected, 0 unexpected\n")
    if mode == "kill-job":
        # The launcher's job shell dies without recording pipeline statuses.
        os.kill(os.getppid(), 9)
    if mode in ("normal", "normal-residue"):
        sys.stdin.buffer.read()
        (art / "exit.status").write_text(
            "compile=0\nlint=0\ntest=0\noverall=0\nclassification=pass\n")
        (art / "manifest.txt").write_text("artifact_completeness=complete\n")
        (art / "ert-summary.txt").write_text(
            "Ran 1 tests, 1 results as expected, 0 unexpected\n")
        if mode == "normal":
            (s / "container").unlink()
    else:
        (s / "ready").touch()
        while True: time.sleep(.02)
elif a[0] == "inspect":
    if not (s / "container").exists(): sys.exit(1)
    label = "foreign-run" if mode == "foreign-label" else (s / "label").read_text()
    print(cid + " " + label)
elif a[0] in ("kill", "stop"):
    if a[-1] != cid: sys.exit(98)
elif a[0] == "rm":
    if a[-1] != cid: sys.exit(98)
    (s / "rm-started").touch()
    if mode == "remove-fails": sys.exit(9)
    if mode == "remove-hangs": time.sleep(30)
    if mode == "remove-slow": time.sleep(1.5)
    (s / "container").unlink(missing_ok=True)
else:
    sys.exit(97)
'''


class Cancellation(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="dorg-cancel-control-")
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.bin = self.base / "bin"
        self.bin.mkdir()
        self.state = self.base / "state"
        self.state.mkdir()
        self.art = self.base / "artifacts"
        for name, source in (
            ("docker", DOCKER),
            ("git", '#!/bin/sh\ncase "$1" in\nrev-parse) printf "%040d\\n" 1;;\nbundle) printf bundle;;\nesac\n'),
        ):
            path = self.bin / name
            path.write_text(source)
            path.chmod(0o755)

    BOUNDARY_TRAP = (
        "trap '[[ $BASH_COMMAND == \"$DORG_TEST_BOUNDARY\" &&"
        " ! -e $DORG_TEST_BOUNDARY_MARK &&"
        " ( -z ${DORG_TEST_BOUNDARY_AFTER_LAUNCH:-} || -n ${pipeline_pid:-} ) ]] &&"
        " { : > \"$DORG_TEST_BOUNDARY_MARK\"; kill -TERM $$; }' DEBUG\n")

    def boundary(self, command, after_launch=False):
        """Return BASH_ENV settings that send TERM to the launcher itself once,
        immediately before the top-level COMMAND (deterministic boundary)."""
        mark = self.base / "boundary-fired"
        path = self.base / "boundary.bash"
        path.write_text(self.BOUNDARY_TRAP)
        env = {"BASH_ENV": str(path), "DORG_TEST_BOUNDARY": command,
               "DORG_TEST_BOUNDARY_MARK": str(mark)}
        if after_launch:
            env["DORG_TEST_BOUNDARY_AFTER_LAUNCH"] = "1"
        return env, mark

    def run_launcher(self, mode="running", sig=signal.SIGTERM, group=False,
                     send=True, repeat=False, bash_env=None):
        env = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                   FAKE_STATE=str(self.state), FAKE_MODE=mode, TMPDIR=str(self.base))
        if bash_env:
            env.update(bash_env)
        # The test's rescue is separate from launcher cleanup and is never
        # counted as a passing observation. It also runs on assertion failure.
        out = self.base / "launcher.log"
        with out.open("w") as log:
            proc = subprocess.Popen(
                [str(ROOT / "tools/dorg-suite-job-local"), "--image", "fake",
                 "--artifacts", str(self.art)], env=env, stdout=log,
                stderr=subprocess.STDOUT, start_new_session=True)
            try:
                if mode != "normal" and send:
                    deadline = time.monotonic() + 8
                    while not (self.state / "ready").exists():
                        if proc.poll() is not None or time.monotonic() > deadline:
                            self.fail("launcher never ready: " + out.read_text())
                        time.sleep(.02)
                    if group:
                        os.killpg(proc.pid, sig)
                    else:
                        proc.send_signal(sig)
                    if repeat:
                        # A second signal while cleanup is removing the
                        # container must not interrupt custody discharge.
                        deadline = time.monotonic() + 8
                        while not (self.state / "rm-started").exists():
                            if proc.poll() is not None or time.monotonic() > deadline:
                                self.fail("cleanup never reached rm: " + out.read_text())
                            time.sleep(.02)
                        proc.send_signal(sig)
                status = proc.wait(timeout=12)
                calls = [json.loads(x) for x in (self.state / "calls").read_text().splitlines()]
                receipt = (self.art / "exit.status").read_text()
                self.assertTrue((self.art / "host-census/after.txt").exists(), out.read_text())
                if mode != "normal":
                    self.assertNotEqual(0, status)
                    self.assertIn("classification=incomplete", receipt)
                    self.assertIn("host_interruption=", receipt)
                    if (self.state / "client").exists():
                        # The container started: its partial output is kept.
                        self.assertEqual("retained-before-interruption\n",
                                         (self.art / "raw.log").read_text())
                        self.assertIn("compile=0", receipt)
                if mode != "normal" and (self.state / "client").exists():
                    # Observe before finally's independent test rescue: the
                    # launcher's own job group cleanup ended the client.
                    with self.assertRaises(ProcessLookupError):
                        os.kill(int((self.state / "client").read_text()), 0)
                if mode == "running":
                    self.assertFalse((self.state / "container").exists())
                return status, receipt, calls
            finally:
                # A corrected launcher creates its own job group; its fake
                # client PID is explicitly owned by this test invocation.
                if (self.state / "client").exists():
                    try:
                        os.kill(int((self.state / "client").read_text()), signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                proc.wait(timeout=3)

    def test_term_cleans_container_and_client_before_test_rescue(self):
        status, _, calls = self.run_launcher(group=True)
        self.assertEqual(143, status)
        self.assertFalse((self.state / "container").exists())
        self.assertTrue(any(c[0] == "rm" and c[-1] == CID for c in calls))

    def test_int_and_hup_cleanup(self):
        # Separate tests below give each signal its own fixture.
        status, _, _ = self.run_launcher(sig=signal.SIGINT)
        self.assertEqual(130, status)
        self.assertFalse((self.state / "container").exists())

    def test_hup_cleanup(self):
        status, _, _ = self.run_launcher(sig=signal.SIGHUP)
        self.assertEqual(129, status)
        self.assertFalse((self.state / "container").exists())

    def test_before_create_does_not_guess_a_container(self):
        _, _, calls = self.run_launcher("before-create")
        self.assertFalse(any(c[0] in ("rm", "stop", "kill") for c in calls))

    def test_foreign_label_never_removed(self):
        _, _, calls = self.run_launcher("foreign-label")
        self.assertFalse(any(c[0] in ("rm", "stop", "kill") for c in calls))
        self.assertTrue((self.state / "container").exists())

    def test_invalid_cid_never_removed(self):
        _, _, calls = self.run_launcher("invalid-cid")
        self.assertFalse(any(c[0] in ("rm", "stop", "kill") for c in calls))

    def test_remove_error_retained(self):
        _, receipt, _ = self.run_launcher("remove-fails")
        self.assertTrue((self.state / "container").exists())
        self.assertIn("host_census=fail", receipt)
        self.assertIn("remove_status=9", (self.art / "host-census/cleanup.txt").read_text())

    def test_remove_hang_is_bounded_and_not_claimed_clean(self):
        _, receipt, _ = self.run_launcher("remove-hangs")
        self.assertTrue((self.state / "container").exists())
        self.assertNotIn("host_census=pass", receipt)
        self.assertRegex((self.art / "host-census/cleanup.txt").read_text(),
                         r"remove_status=(124|137)")

    def test_cancelled_green_receipt_is_incomplete(self):
        status, receipt, _ = self.run_launcher("green-then-wait")
        self.assertEqual(143, status)
        lines = receipt.splitlines()
        self.assertIn("classification=incomplete", lines)
        self.assertNotIn("classification=pass", lines)
        self.assertIn("receipt_classification=pass", lines)
        self.assertIn("host_interruption=TERM", receipt)
        self.assertFalse((self.state / "container").exists())

    def test_abnormal_job_exit_is_cleaned_and_incomplete(self):
        status, receipt, calls = self.run_launcher("kill-job", send=False)
        self.assertNotEqual(0, status)
        self.assertIn("host_interruption=abnormal-exit", receipt)
        self.assertFalse((self.state / "container").exists())
        self.assertTrue(any(c[0] == "rm" and c[-1] == CID for c in calls))

    def test_repeated_signal_does_not_interrupt_cleanup(self):
        status, receipt, _ = self.run_launcher("remove-slow", repeat=True)
        self.assertEqual(143, status)
        self.assertFalse((self.state / "container").exists())
        self.assertIn("remove_status=0", (self.art / "host-census/cleanup.txt").read_text())
        self.assertIn("host_interruption=TERM", receipt)

    def test_lost_cidfile_is_unknown_not_guessed(self):
        # Creation race: the container exists but its CID never reached the
        # cidfile.  Cleanup must not discover a target; custody is unknown.
        status, receipt, calls = self.run_launcher("no-cidfile")
        self.assertEqual(143, status)
        self.assertFalse(any(c[0] in ("rm", "stop", "kill") for c in calls))
        self.assertTrue((self.state / "container").exists())
        self.assertIn("identity=missing", (self.art / "host-census/cleanup.txt").read_text())
        self.assertIn("classification=incomplete", receipt)
        self.assertNotIn("host_census=pass", receipt)

    def test_signal_at_launch_boundary_is_held_until_ownership(self):
        # TERM lands after the job is started but before its group is
        # recorded; it must be held, then discharged through custody cleanup.
        env_file, mark = self.boundary('pipeline_pid=$!')
        status, receipt, calls = self.run_launcher(send=False, bash_env=env_file)
        self.assertTrue(mark.exists())
        self.assertEqual(143, status)
        self.assertIn("classification=incomplete", receipt.splitlines())
        self.assertIn("host_interruption=TERM", receipt.splitlines())
        cleanup = (self.art / "host-census/cleanup.txt").read_text()
        self.assertRegex(cleanup, r"(?m)^pipeline_pid=[0-9]+$")
        if (self.state / "container").exists():
            # Created but not yet identified: reported, never guessed.
            self.assertIn("identity=missing", cleanup)
            self.assertNotIn("host_census=pass", receipt)
        else:
            self.assertNotIn("identity=unverified", cleanup)

    def test_signal_at_final_exit_keeps_committed_outcome(self):
        env_file, mark = self.boundary('exit "$status"')
        status, receipt, calls = self.run_launcher("normal", bash_env=env_file)
        self.assertTrue(mark.exists())
        self.assertEqual(0, status)
        lines = receipt.splitlines()
        self.assertIn("classification=pass", lines)
        self.assertNotIn("host_interruption=TERM", lines)
        self.assertFalse((self.art / "host-census/cleanup.txt").exists())

    def test_signal_after_verdict_rederives_from_container_receipt(self):
        # The container exits green but leaves residue, so the launcher's own
        # verdict (fail) differs from the container receipt (pass).
        env_file, mark = self.boundary('[[ "$positive_control" == true ]]',
                                       after_launch=True)
        status, receipt, calls = self.run_launcher("normal-residue", send=False,
                                                   bash_env=env_file)
        self.assertTrue(mark.exists())
        self.assertEqual(143, status)
        lines = receipt.splitlines()
        self.assertIn("classification=incomplete", lines)
        self.assertIn("host_interruption=TERM", lines)
        # The container's own green receipt, not the launcher's earlier verdict.
        self.assertIn("receipt_classification=pass", lines)
        self.assertIn("receipt_overall=0", lines)
        self.assertEqual(1, sum(1 for x in lines if x.startswith("classification=")))

    def test_absent_timeout_refuses_before_docker(self):
        minimal = self.base / "minimal-bin"
        minimal.mkdir()
        for name in ("git", "docker"):
            (minimal / name).symlink_to(self.bin / name)
        grep = next(p for p in ("/usr/bin/grep", "/bin/grep") if os.path.exists(p))
        (minimal / "grep").symlink_to(grep)
        env = dict(os.environ, PATH=str(minimal), FAKE_STATE=str(self.state),
                   FAKE_MODE="normal", TMPDIR=str(self.base))
        result = subprocess.run(
            ["/bin/bash", str(ROOT / "tools/dorg-suite-job-local"), "--image", "fake",
             "--artifacts", str(self.art)], env=env, capture_output=True,
            text=True, timeout=12)
        self.assertEqual(69, result.returncode, result.stderr)
        self.assertIn("GNU timeout", result.stderr)
        self.assertFalse((self.state / "calls").exists())
        self.assertFalse(self.art.exists())

    def test_non_gnu_timeout_refuses_before_docker(self):
        fake = self.bin / "timeout"
        fake.write_text("#!/bin/sh\necho 'timeout (BSD) 1.0'\nexit 0\n")
        fake.chmod(0o755)
        # FAKE_MODE=normal: a launcher that fails to refuse still finishes
        # rather than leaving a hanging fake client behind.
        env = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                   FAKE_STATE=str(self.state), FAKE_MODE="normal", TMPDIR=str(self.base))
        result = subprocess.run(
            [str(ROOT / "tools/dorg-suite-job-local"), "--image", "fake",
             "--artifacts", str(self.art)], env=env, capture_output=True,
            text=True, timeout=12)
        self.assertEqual(69, result.returncode)
        self.assertIn("GNU timeout", result.stderr)
        self.assertFalse((self.state / "calls").exists())
        self.assertFalse(self.art.exists())

    def test_normal_completion_unchanged(self):
        status, receipt, calls = self.run_launcher("normal")
        self.assertEqual(0, status)
        self.assertIn("classification=pass", receipt)
        self.assertFalse(any(c[0] in ("rm", "stop", "kill") for c in calls))


if __name__ == "__main__":
    unittest.main()
