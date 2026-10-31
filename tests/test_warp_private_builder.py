#!/usr/bin/env python3
"""Test private-build injection without private media or VESC resources."""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import stat
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "warp_private_builder", ROOT / "tools/warp-sound/build_private.py")
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)


class PrivateBuilderTests(unittest.TestCase):
    def setUp(self):
        self.default_source = (ROOT / "app.lisp").read_text()

    def test_default_app_has_no_sample_import_or_song_seven_dispatch(self):
        self.assertNotIn("samples/warp-sample-", self.default_source)
        self.assertNotIn("warp-samples-enabled", self.default_source)
        self.assertNotIn("warp-song-number", self.default_source)
        self.assertNotIn("(= cmd warp-song-number)", self.default_source)

    def test_private_overlay_injects_all_chunks_and_generation_owned_controls(self):
        source = builder.injected_app(self.default_source)
        imports = re.findall(
            r'\(import "samples/warp-sample-(\d+)\.s8" \'warp-sample-\d+\)', source)
        self.assertEqual(sorted(map(int, imports)), list(range(12)))
        self.assertIn("(def warp-samples-enabled true)", source)
        self.assertIn("(def warp-song-number 240)", source)
        self.assertIn("(def music-play-generation 0)", source)
        self.assertIn("((and warp-samples-enabled (= cmd warp-song-number))", source)
        self.assertNotIn("@private-warp-", source)
        for name in builder.PRIVATE_FUNCTIONS:
            matches = re.findall(r"\(defun " + re.escape(name) + r"\b", source)
            self.assertEqual(len(matches), 1, name)
        self.assertIn("(advance-music-generation)", builder.PRIVATE_FUNCTIONS["stop-music"])
        self.assertEqual(len(re.findall(r"^\(def music-play-generation 0\)$", source, re.M)), 1)
        self.assertEqual(source.count("(defun advance-music-generation "), 1)
        self.assertIn("(music-track-finished generation parser-idx)", source)
        self.assertIn("(var generation (advance-music-generation))",
                      builder.PRIVATE_FUNCTIONS["play-current-song"])
        self.assertIn("(var generation (advance-music-generation))",
                      builder.PRIVATE_FUNCTIONS["play-axelf"])

    def test_existing_sample_catalog_entry_is_preserved(self):
        existing = {"number": 240, "name": "Space Whoosh", "source": "builtin-samples", "default": "tng-warp2"}
        base = [{"number": 1, "name": "Axel F"}, existing]
        self.assertEqual(builder.stage_catalog(base), base)
        self.assertEqual(base[1], existing)
        self.assertEqual(builder.stage_catalog([base[0]])[-1], builder.ENTRY)
        with self.assertRaisesRegex(ValueError, "not a resident sample"):
            builder.stage_catalog([{"number": 240, "name": "other", "tracks": [{"file": "a"}]}])
        with self.assertRaisesRegex(ValueError, "more than once"):
            builder.stage_catalog([existing, existing])
        with self.assertRaisesRegex(ValueError, "not a resident sample"):
            builder.stage_catalog([{"number": 240, "source": "builtin-samples", "tracks": [{"file": "a"}]}])

    def test_explicit_legacy_migration_and_custom_sample_number(self):
        legacy = {"number": 7, "name": "TNG Warp 2", "source": "builtin-samples", "default": "tng-warp2"}
        with self.assertRaisesRegex(ValueError, 'legacy sample at 7'):
            builder.stage_catalog([legacy])
        moved = builder.stage_catalog([legacy], 255, migrate_legacy=True)
        self.assertEqual(moved, [dict(legacy, number=255)])
        self.assertEqual(legacy['number'], 7)
        with self.assertRaisesRegex(ValueError, 'already in use'):
            builder.stage_catalog([legacy, {'number': 240}], migrate_legacy=True)
        for number in (0, 7, 239, 256):
            with self.assertRaisesRegex(ValueError, '240-255'):
                builder.injected_app(self.default_source, number)
        self.assertIn('(def warp-song-number 255)', builder.injected_app(self.default_source, 255))

    def test_upgrade_existing_pcm_preserves_target_and_applies_merged_upload_fix(self):
        original = builder.injected_app(self.default_source).replace('(def warp-song-number 240)', '(def warp-song-number 7)')
        original = re.sub(r'\(if\s+\(or music-playing music-starting\)\s*\{\s*'
                          r'\(stop-music\)\s*\(send-music-status\)\s*\}\)',
                          '(if music-playing (stop-music))', original)
        original = original.replace('(def shutdown-threshold 2.48)', '(def shutdown-threshold 2.50)')
        updated = builder.upgrade_existing_app(original, 240)
        self.assertIn('(def warp-song-number 240)', updated)
        self.assertIn('(def shutdown-threshold 2.50)', updated)
        self.assertIn('(if (or music-playing music-starting) (stop-music))', updated)
        self.assertEqual(builder.extract_defun(original, 'check-shutdown'), builder.extract_defun(updated, 'check-shutdown'))
        with self.assertRaisesRegex(ValueError, 'enabled resident sample'):
            builder.upgrade_existing_app(self.default_source, 240)
        with self.assertRaisesRegex(ValueError, 'song number'):
            builder.upgrade_existing_app(original, 239)

    def test_pcm_upgrade_preserves_new_upload_status_hook(self):
        original = builder.injected_app(self.default_source)
        updated = builder.upgrade_existing_app(original, 255)
        self.assertIn('(def warp-song-number 255)', updated)
        self.assertEqual(builder.extract_defun(original, 'handle-upload-begin'),
                         builder.extract_defun(updated, 'handle-upload-begin'))
        self.assertIn('(send-music-status)', builder.extract_defun(updated, 'handle-upload-begin'))

    def test_custom_target_logic_is_preserved_and_music_mismatch_rejected(self):
        custom = self.default_source.replace('(def shutdown-threshold 2.48)', '(def shutdown-threshold 2.50)')
        self.assertNotEqual(custom, self.default_source)
        result = builder.injected_app(custom)
        self.assertIn('(def shutdown-threshold 2.50)', result)
        self.assertEqual(builder.extract_defun(custom, 'check-shutdown'), builder.extract_defun(result, 'check-shutdown'))
        self.assertEqual(builder.extract_defun(custom, 'play-current-song'), builder.extract_defun(result, 'play-current-song'))
        changed_music = custom.replace('(defun stop-music () {', '(defun stop-music () { (print "target")', 1)
        self.assertNotEqual(changed_music, custom)
        with self.assertRaisesRegex(ValueError, 'target music function differs'):
            builder.injected_app(changed_music)
        with self.assertRaisesRegex(ValueError, 'injection point'):
            builder.injected_app(custom.replace('; @private-warp-init', ''))

    def test_all_target_resources_are_preserved(self):
        flash = builder.load_module(ROOT / 'tunu-flash.py', 'pcm_target_flash')
        source = self.default_source + '\n(import "extra.bin" \'target-extra)\n'
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            metadata = []
            for _, tag in flash.source_imports(source.encode()):
                data = ('resource-' + tag).encode()
                (root / ('installed-' + tag + '.bin')).write_bytes(data)
                metadata.append({'tag': tag, 'length': len(data) + 1, 'sha256': hashlib.sha256(data).hexdigest()})
            (root / 'installed-imports.json').write_text(json.dumps(metadata))
            self.assertEqual(set(builder.stock_resource_overrides(root, source, flash)),
                             {entry['tag'] for entry in metadata})
            (root / 'installed-target-extra.bin').write_bytes(b'x' * len(b'resource-target-extra'))
            with self.assertRaisesRegex(ValueError, 'hash mismatch'):
                builder.stock_resource_overrides(root, source, flash)
            (root / 'installed-imports.json').write_text(json.dumps(metadata[:-1]))
            with self.assertRaisesRegex(ValueError, 'missing preserved resources'):
                builder.stock_resource_overrides(root, source, flash)

    def test_reusable_tools_have_no_machine_specific_paths(self):
        for path in (ROOT / "tools/warp-sound/build_private.py",
                     ROOT / "tools/warp-sound/convert_audio.py",
                     ROOT / "tools/warp-sound/README.md"):
            text = path.read_text()
            self.assertNotIn("/home/", text)
            self.assertNotIn(".local/state", text)

    def test_new_output_directory_is_private_even_with_permissive_umask(self):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / "private" / "nested" / "output"
            previous = os.umask(0)
            try:
                result = builder.prepare_private_output(output)
            finally:
                os.umask(previous)
            self.assertEqual(result, output.resolve())
            for directory in (output.parent.parent, output.parent, output):
                self.assertEqual(stat.S_IMODE(directory.stat().st_mode), 0o700)

    def test_existing_output_with_group_access_is_rejected_not_changed(self):
        with tempfile.TemporaryDirectory() as temp:
            output = Path(temp) / "existing-output"
            output.mkdir(mode=0o755)
            output.chmod(0o755)
            with self.assertRaisesRegex(ValueError, "accessible by group or others"):
                builder.prepare_private_output(output)
            self.assertEqual(stat.S_IMODE(output.stat().st_mode), 0o755)

    def test_private_input_and_output_paths_cannot_target_repository(self):
        with self.assertRaisesRegex(ValueError, "outside the repository"):
            builder.require_private_path(ROOT / "private-media.s8", "sample")

    def test_function_names_do_not_match_longer_prefixes(self):
        source = '(defun status () true)\n(defun status-locked () false)'
        self.assertEqual(builder.extract_defun(source, 'status'), '(defun status () true)')
        self.assertEqual(builder.replace_defun(source, 'status', '(defun status () 1)'),
                         '(defun status () 1)\n(defun status-locked () false)')

    def test_function_replacement_skips_comments_and_parentheses_in_strings(self):
        source = '(defun target () { (print "(ignored)") ; ) ignored\n true })\n(defun other () nil)'
        changed = builder.replace_defun(source, "target", "(defun target () false)")
        self.assertEqual(changed,
                         '(defun target () false)\n(defun other () nil)')


if __name__ == "__main__":
    unittest.main()
