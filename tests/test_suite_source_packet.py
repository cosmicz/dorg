"""Exercise source admission with real, tiny Git repositories; no runtime suite."""

import copy
import hashlib
import io
import json
import os
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest


IMPORTER = Path(__file__).resolve().parents[1] / "tools/dorg-suite-job-source.py"


class SourcePacketTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="source-packet-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.env = {"PATH": os.environ["PATH"], "LANG": "C",
                    "GIT_CONFIG_NOSYSTEM": "1", "GIT_CONFIG_GLOBAL": "/dev/null"}
        self.rows = []
        self.bundles = {}
        nested = self.repo("nested")
        child = self.repo("child", [("nested", nested)])
        root = self.repo("root", [("lib/child", child)])
        # This is deliberately untracked.  A source packet is reconstructed
        # only from declared Git bundles, never by copying a checkout's work
        # tree, so an active-install marker cannot reach CI/source admission.
        self.installed_marker = root / ".installed-root"
        self.installed_marker.write_text("active installed runtime\n")
        for number, (path, repo) in enumerate([
                (".", root), ("lib/child", child), ("lib/child/nested", nested)]):
            bundle = self.root / f"{number:02}.bundle"
            self.git(repo, "bundle", "create", str(bundle), "HEAD")
            content = bundle.read_bytes()
            self.bundles[bundle.name] = content
            self.rows.append({"path": path, "commit": self.git(repo, "rev-parse", "HEAD"),
                              "tree": self.git(repo, "rev-parse", "HEAD^{tree}"),
                              "bundle": bundle.name, "bytes": len(content),
                              "sha256": hashlib.sha256(content).hexdigest()})
        self.manifest = {"schema": 1, "candidate": self.rows[0]["commit"],
                         "components": self.rows}

    def git(self, cwd, *args):
        return subprocess.check_output(
            ["git", "-c", "core.hooksPath=/dev/null", "-C", str(cwd), *args],
            env=self.env, stderr=subprocess.DEVNULL, text=True).strip()

    def repo(self, name, links=()):
        path = self.root / name
        path.mkdir()
        self.git(path, "init", "-q")
        (path / "content.txt").write_text(name + "\n")
        self.git(path, "add", "content.txt")
        for relative, target in links:
            self.git(path, "update-index", "--add", "--cacheinfo", "160000",
                     self.git(target, "rev-parse", "HEAD"), relative)
        self.git(path, "-c", "user.name=fixture", "-c", "user.email=fixture@example.invalid",
                 "commit", "-qm", "fixture")
        return path

    def run_packet(self, mutate=None, extra=None, requested_tree=None):
        manifest = copy.deepcopy(self.manifest)
        if mutate:
            mutate(manifest)
        packet = self.root / "source.tar"
        with tarfile.open(packet, "w", format=tarfile.USTAR_FORMAT) as archive:
            files = {"manifest.json": json.dumps(manifest).encode()}
            files.update({r["bundle"]: self.bundles[r["bundle"]]
                          for r in manifest["components"]})
            for name, data in files.items():
                entry = tarfile.TarInfo(name)
                entry.size = len(data)
                archive.addfile(entry, io.BytesIO(data))
            if extra:
                archive.addfile(extra)
        receipt = self.root / "receipt.json"
        result = subprocess.run(
            [sys.executable, str(IMPORTER), "--packet", str(packet),
             "--commit", self.rows[0]["commit"],
             "--tree", requested_tree or self.rows[0]["tree"],
             "--destination", str(self.root / "checkout"), "--receipt", str(receipt)],
            env=self.env, text=True, capture_output=True, timeout=30)
        return result, receipt

    def refused(self, **kwargs):
        result, receipt = self.run_packet(**kwargs)
        self.assertEqual(2, result.returncode, result.stdout + result.stderr)
        self.assertIn("source packet refused:", result.stderr)
        self.assertFalse(receipt.exists())

    def test_exact_recursive_reconstruction(self):
        result, receipt = self.run_packet()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        actual = json.loads(receipt.read_text())
        self.assertEqual(self.rows, actual["components"])
        for row in self.rows:
            path = self.root / "checkout" / row["path"]
            self.assertEqual(row["commit"], self.git(path, "rev-parse", "HEAD"))
            self.assertEqual(row["tree"], self.git(path, "rev-parse", "HEAD^{tree}"))
        self.assertEqual("nested\n", (self.root / "checkout/lib/child/nested/content.txt").read_text())

    def test_untracked_installed_marker_never_enters_packet(self):
        self.assertTrue(self.installed_marker.is_file())
        result, receipt = self.run_packet()
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(receipt.exists())
        self.assertFalse((self.root / "checkout/.installed-root").exists())

    def test_bad_bundle_hash(self):
        self.refused(mutate=lambda m: m["components"][1].update(sha256="0" * 64))

    def test_wrong_requested_tree(self):
        self.refused(requested_tree="0" * 40)

    def test_wrong_dependency_commit(self):
        self.refused(mutate=lambda m: m["components"][1].update(commit=self.rows[2]["commit"]))

    def test_missing_recursive_component(self):
        self.refused(mutate=lambda m: m["components"].pop())

    def test_unlinked_extra_component(self):
        self.refused(mutate=lambda m: m["components"][2].update(path="other"))

    def test_duplicate_component(self):
        self.refused(mutate=lambda m: m["components"].append(copy.deepcopy(m["components"][1])))

    def test_absolute_destination_component(self):
        self.refused(mutate=lambda m: m["components"][1].update(path="/tmp/escaped"))

    def test_parent_traversal_component(self):
        self.refused(mutate=lambda m: m["components"][1].update(path="../escaped"))

    def test_unsafe_archive_member(self):
        self.refused(extra=tarfile.TarInfo("../escaped"))
        self.assertFalse((self.root.parent / "escaped").exists())

    def test_archive_symlink(self):
        entry = tarfile.TarInfo("link")
        entry.type = tarfile.SYMTYPE
        entry.linkname = "/tmp"
        self.refused(extra=entry)


if __name__ == "__main__":
    unittest.main()
