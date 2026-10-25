#!/usr/bin/env python3
"""Exercise the private venue command against real disposable Git repos."""

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
VENUE = ROOT / "tools/dorg-private-venue"
PRIVATE_REPO = "cosmicz/DOrg.oc"


class PrivateVenueTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="dorg-private-venue-")
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.source = self.base / "source"
        self.public = self.base / "public.git"
        self.private = self.base / "private.git"
        self.source.mkdir()
        self.git("init", "-q", cwd=self.source)
        self.git("config", "user.name", "Venue Test")
        self.git("config", "user.email", "venue@example.invalid")
        (self.source / "file.txt").write_text("first\n")
        self.git("add", "file.txt")
        self.git("commit", "-q", "-m", "first")
        self.sha = self.git("rev-parse", "HEAD").stdout.strip()
        self.git("init", "-q", "--bare", str(self.public))
        self.git("init", "-q", "--bare", str(self.private))
        self.git("remote", "add", "origin", str(self.public))
        self.git("remote", "add", "private", "git@github.com:cosmicz/DOrg.oc.git")
        self.ssh = self.base / "fake-ssh"
        self.ssh.write_text(
            "#!/usr/bin/env python3\n"
            "import os, sys\n"
            "command = ' '.join(sys.argv[1:])\n"
            "program = 'git-receive-pack' if 'git-receive-pack' in command else 'git-upload-pack'\n"
            "repo = os.environ['VENUE_PRIVATE_BARE'] if 'dorg.oc.git' in command.lower() else os.environ['VENUE_PUBLIC_BARE']\n"
            "os.execvp(program, [program, repo])\n"
        )
        self.ssh.chmod(0o755)
        self.gh_log = self.base / "gh-argv.jsonl"
        fake_bin = self.base / "bin"
        fake_bin.mkdir()
        gh = fake_bin / "gh"
        gh.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, sys\n"
            "args = sys.argv[1:]\n"
            "with open(os.environ['VENUE_GH_LOG'], 'a') as out: out.write(json.dumps(args) + '\\n')\n"
            "if '-R' not in args or args[args.index('-R') + 1] != 'cosmicz/DOrg.oc': sys.exit(90)\n"
            "print('fake gh response')\n"
        )
        gh.chmod(0o755)
        self.env = dict(os.environ)
        self.env.update(
            GIT_SSH_COMMAND=str(self.ssh),
            VENUE_PRIVATE_BARE=str(self.private),
            VENUE_PUBLIC_BARE=str(self.public),
            VENUE_GH_LOG=str(self.gh_log),
            PATH=str(fake_bin) + os.pathsep + self.env["PATH"],
        )

    def git(self, *args, cwd=None):
        return subprocess.run(
            ["git", *args], cwd=cwd or self.source, text=True,
            capture_output=True, check=True,
        )

    def venue(self, *args):
        return subprocess.run(
            [str(VENUE), *args], cwd=self.source, env=self.env,
            text=True, capture_output=True,
        )

    def ref(self, repo, name):
        result = subprocess.run(
            ["git", "--git-dir", str(repo), "rev-parse", "--verify", name],
            text=True, capture_output=True,
        )
        return result.stdout.strip() if result.returncode == 0 else None

    def test_gate_push_changes_only_private_ref_and_prints_readback(self):
        ref = f"refs/heads/ci/abcde-{self.sha[:12]}"
        self.assertIsNone(self.ref(self.private, ref))
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.ref(self.private, ref), self.sha)
        self.assertIsNone(self.ref(self.public, ref))
        self.assertIn("remote=private", result.stdout)
        self.assertIn("effective_url=git@github.com:cosmicz/DOrg.oc.git", result.stdout)
        self.assertIn(f"sha={self.sha}", result.stdout)
        self.assertIn(f"ref={ref}", result.stdout)
        self.assertIn(f"readback={self.sha}", result.stdout)

    def test_origin_only_and_misleading_remote_refuse(self):
        self.git("remote", "remove", "private")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(self.ref(self.public, f"refs/heads/ci/abcde-{self.sha[:12]}"))
        self.git("remote", "rename", "origin", "private")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)

    def test_ambiguous_private_remotes_refuse(self):
        self.git("remote", "add", "second", "ssh://git@github.com/cosmicz/DOrg.oc.git")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(self.ref(self.private, f"refs/heads/ci/abcde-{self.sha[:12]}"))

    def test_private_fetch_with_public_push_refuses(self):
        self.git("remote", "set-url", "--push", "private", "git@github.com:cosmicz/dorg.git")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(self.ref(self.public, f"refs/heads/ci/abcde-{self.sha[:12]}"))

    def test_one_remote_with_multiple_push_urls_refuses(self):
        self.git("remote", "set-url", "--push", "private",
                 "git@github.com:cosmicz/DOrg.oc.git")
        self.git("remote", "set-url", "--add", "--push", "private",
                 "git@github.com:cosmicz/dorg.git")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.assertIsNone(self.ref(self.private, f"refs/heads/ci/abcde-{self.sha[:12]}"))
        self.assertIsNone(self.ref(self.public, f"refs/heads/ci/abcde-{self.sha[:12]}"))

    def test_hostile_push_rewrite_and_other_owner_refuse(self):
        self.git("config", "url.git@github.com:cosmicz/dorg.git.pushInsteadOf",
                 "git@github.com:cosmicz/DOrg.oc.git")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)
        self.git("config", "--unset", "url.git@github.com:cosmicz/dorg.git.pushInsteadOf")
        self.git("remote", "set-url", "private", "git@github.com:another/foo.oc.git")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertNotEqual(result.returncode, 0)

    def test_same_identity_https_to_ssh_rewrite_passes(self):
        self.git("remote", "set-url", "private",
                 "https://github.com/cosmicz/DOrg.oc.git")
        self.git("config", "url.git@github.com:.pushInsteadOf",
                 "https://github.com/")
        result = self.venue("push-gate", "abcde", self.sha)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("effective_url=git@github.com:cosmicz/DOrg.oc.git",
                      result.stdout)
        self.assertEqual(self.ref(self.private,
                                  f"refs/heads/ci/abcde-{self.sha[:12]}"),
                         self.sha)
        self.assertIsNone(self.ref(self.public,
                                   f"refs/heads/ci/abcde-{self.sha[:12]}"))

    def test_trunk_push_is_not_forced(self):
        first = self.venue("push-trunk", self.sha)
        self.assertEqual(first.returncode, 0, first.stderr)
        self.assertEqual(self.ref(self.private, "refs/heads/trunk"), self.sha)
        self.git("checkout", "-q", "--orphan", "alternate")
        (self.source / "file.txt").write_text("unrelated\n")
        self.git("add", "file.txt")
        self.git("commit", "-q", "-m", "unrelated")
        alternate = self.git("rev-parse", "HEAD").stdout.strip()
        second = self.venue("push-trunk", alternate)
        self.assertNotEqual(second.returncode, 0)
        self.assertEqual(self.ref(self.private, "refs/heads/trunk"), self.sha)

    def test_bad_inputs_refuse_without_push(self):
        for args in (("push-gate", "abcde", "HEAD"),
                     ("push-gate", "bad/name", self.sha),
                     ("push-gate", "abcde", self.sha, "origin")):
            with self.subTest(args=args):
                self.assertNotEqual(self.venue(*args).returncode, 0)
        self.assertIsNone(self.ref(self.private, f"refs/heads/ci/abcde-{self.sha[:12]}"))

    def test_gh_operations_pin_private_repository(self):
        commands = (("dispatch", "abcde", self.sha),
                    ("list", "abcde", self.sha),
                    ("view", "123456"),
                    ("download", "123456", str(self.base / "artifacts")))
        for args in commands:
            with self.subTest(args=args):
                result = self.venue(*args)
                self.assertEqual(result.returncode, 0, result.stderr)
        calls = [json.loads(line) for line in self.gh_log.read_text().splitlines()]
        self.assertEqual(len(calls), len(commands))
        for call in calls:
            self.assertIn("-R", call)
            self.assertEqual(call[call.index("-R") + 1], PRIVATE_REPO)


if __name__ == "__main__":
    unittest.main()
