#!/usr/bin/env python3
"""Reconstruct declared Git bundles without following submodule remote URLs.

This is source admission for the existing suite job, not a test executor.
The archive is never extracted: only declared regular bundle members are copied
into owned temporary files.  No make command runs here.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile


def require(condition, message):
    if not condition:
        raise ValueError(message)


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        require(key not in result, "duplicate JSON key")
        result[key] = value
    return result


def digest(path):
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(block)
    return value.hexdigest()


def safe_path(value):
    require(isinstance(value, str) and value, "invalid component path")
    path = PurePosixPath(value)
    require(value == "." or (not path.is_absolute() and str(path) == value
                            and all(p not in (".", "..", ".git") for p in path.parts)
                            and "\\" not in value and not any(ord(c) < 32 for c in value)),
            "unsafe component path")


def git(cwd, *args):
    # No inherited Git directory, global URL rewrite, credential or hook setup.
    env = {"PATH": os.environ["PATH"], "LANG": "C",
           "GIT_CONFIG_NOSYSTEM": "1", "GIT_CONFIG_GLOBAL": "/dev/null",
           "GIT_TERMINAL_PROMPT": "0", "GIT_ALLOW_PROTOCOL": "file"}
    result = subprocess.run(
        ["git", "-c", "core.hooksPath=/dev/null", "-C", str(cwd), *args],
        env=env, capture_output=True, timeout=120)
    require(result.returncode == 0, "git failed: " + result.stderr.decode(errors="replace"))
    return result.stdout


def read_manifest(archive):
    members = archive.getmembers()
    names = [member.name for member in members]
    require(len(names) == len(set(names)), "duplicate archive member")
    require(all(member.isfile() and "/" not in member.name
                and "\\" not in member.name for member in members),
            "unsafe or nonregular archive member")
    require("manifest.json" in names, "manifest missing")
    require(archive.getmember("manifest.json").size <= 1024 * 1024, "manifest too large")
    with archive.extractfile("manifest.json") as stream:
        manifest = json.load(stream, object_pairs_hook=unique_object)
    require(isinstance(manifest, dict), "manifest must be an object")
    require(type(manifest.get("schema")) is int and manifest["schema"] == 1,
            "unsupported manifest schema")
    rows = manifest.get("components")
    require(isinstance(rows, list) and rows, "empty components")
    declared = {}
    bundles = set()
    for row in rows:
        require(isinstance(row, dict), "component must be an object")
        safe_path(row["path"])
        require(row["path"] not in declared, "duplicate component")
        for field, length in (("commit", 40), ("tree", 40), ("sha256", 64)):
            require(isinstance(row[field], str)
                    and re.fullmatch(r"[0-9a-f]{%d}" % length, row[field]),
                    "invalid component " + field)
        require(re.fullmatch(r"[0-9]{2,4}\.bundle", row["bundle"]), "invalid bundle name")
        require(row["bundle"] not in bundles, "duplicate bundle")
        require(type(row["bytes"]) is int and row["bytes"] > 0, "invalid bundle size")
        declared[row["path"]] = row
        bundles.add(row["bundle"])
    require(set(names) == bundles | {"manifest.json"}, "archive membership mismatch")
    return manifest, rows, declared


def restore(packet, destination, commit, tree, receipt):
    require(re.fullmatch(r"[0-9a-f]{40}", commit), "invalid requested commit")
    require(re.fullmatch(r"[0-9a-f]{40}", tree), "invalid requested tree")
    require(not destination.exists() and not destination.is_symlink(), "destination exists")
    require(not receipt.exists(), "receipt exists")
    with tarfile.open(packet, "r:*") as archive, tempfile.TemporaryDirectory(
            prefix="suite-source-") as temporary:
        manifest, rows, declared = read_manifest(archive)
        require("." in declared and declared["."]["commit"] == commit
                and declared["."]["tree"] == tree and manifest.get("candidate") == commit,
                "requested root identity mismatch")
        for row in rows:
            require(archive.getmember(row["bundle"]).size == row["bytes"], "bundle size mismatch")
            target = Path(temporary) / row["bundle"]
            with archive.extractfile(row["bundle"]) as source, target.open("xb") as output:
                shutil.copyfileobj(source, output)
            require(digest(target) == row["sha256"], "bundle hash mismatch")

        links = {}
        destination.parent.mkdir(parents=True, exist_ok=True)
        for path in sorted(declared, key=lambda p: (0 if p == "." else len(PurePosixPath(p).parts), p)):
            row = declared[path]
            target = destination if path == "." else destination / path
            if path != ".":
                require(links.get(path) == row["commit"], "component is not its parent's exact gitlink")
                require(target.resolve() == destination.resolve() / path,
                        "component traverses a symlink")
            git(destination.parent, "clone", "--quiet", str(Path(temporary) / row["bundle"]), str(target))
            for suffix, expected in (("HEAD", row["commit"]), ("HEAD^{tree}", row["tree"])):
                require(git(target, "rev-parse", suffix).decode().strip() == expected,
                        "reconstructed " + suffix + " mismatch")
            git(target, "fsck", "--connectivity-only")
            for entry in git(target, "ls-tree", "-rz", "HEAD").split(b"\0"):
                if not entry:
                    continue
                metadata, name = entry.split(b"\t", 1)
                mode, kind, sha = metadata.decode().split()
                if mode == "160000":
                    relative = name.decode()
                    full = relative if path == "." else path + "/" + relative
                    safe_path(full)
                    require(kind == "commit", "invalid gitlink kind")
                    links[full] = sha
        require(set(links) == set(declared) - {"."}, "recursive gitlink inventory mismatch")
        for path in declared:
            require(not git(destination / path, "status", "--porcelain", "--untracked-files=all"),
                    "dirty reconstructed component")
    receipt.write_text(json.dumps({"schema": 1, "source_packet_sha256": digest(packet),
                                   "candidate": commit, "tree": tree, "components": rows,
                                   "complete_gitlinks": True}, indent=2) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("packet", "commit", "tree", "destination", "receipt"):
        parser.add_argument("--" + name, required=True)
    args = parser.parse_args()
    try:
        restore(Path(args.packet), Path(args.destination), args.commit, args.tree, Path(args.receipt))
    except (ValueError, KeyError, TypeError, OSError, tarfile.TarError, subprocess.SubprocessError) as error:
        print("source packet refused: " + str(error), file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
