import hashlib
import importlib.util
import json
import os
import shutil
import subprocess
from pathlib import Path
import re
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('engine_builder', ROOT / 'tools/engine-sound/build.py')
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)


class EngineBuilderTests(unittest.TestCase):
    def legacy_source(self):
        source = (ROOT / 'app.lisp').read_text()
        names = re.findall(r'^\(defun ((?:engine-[A-Za-z0-9_-]+)|handle-engine-command|send-engine-status(?:-locked)?)\s', source, re.M)
        for name in names:
            source = builder.warp.replace_defun(source, name, '')
        source = re.sub(r'^\(def engine-[A-Za-z0-9_-]+ .+\)(?:[^\n]*)\n', '', source, flags=re.M)
        for line in ('    (engine-init)\n', '    (spawn 300 engine-loop)\n',
                     '    (def dataArray_0x7ED (array-create 4))\n',
                     '    (if (= id 0x4EAu32) (handle-engine-command data))\n',
                     '        (send-engine-status)\n'):
            source = source.replace(line, '')
        source = source.replace('(list abs sin loopforeach looprange loopwhile)',
                                '(list abs loopforeach looprange loopwhile)')
        source = re.sub(r'\(def sw-app-version \d+\)', '(def sw-app-version 1)', source)
        source = builder.warp.replace_defun(source, 'send-status2', '(defun send-status2 () 25)')
        return source

    def inputs(self, root):
        backup = root / 'backup'
        backup.mkdir()
        source = self.legacy_source()
        (backup / 'before.lisp').write_text(source)
        metadata = []
        for _, tag in builder.flash.source_imports(source.encode()):
            raw = ('target-resource:' + tag).encode()
            (backup / ('installed-' + tag + '.bin')).write_bytes(raw)
            metadata.append({'tag': tag, 'length': len(raw) + 1, 'sha256': hashlib.sha256(raw).hexdigest()})
        (backup / 'installed-imports.json').write_text(json.dumps(metadata))
        return backup, source

    def test_target_logic_and_every_import_are_preserved(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            backup, original = self.inputs(root)
            output = root / 'output'
            metadata = builder.build(backup, output)
            source, imports = builder.flash.unpack_blob(builder.flash.validate_image((output / 'engine.img').read_bytes()))
            source = source.decode()
            version = re.search(r'\(def sw-app-version (\d+)\)', (ROOT / 'app.lisp').read_text())[1]
            self.assertIn('(def sw-app-version %s)' % version, source)
            self.assertIn('(defun send-status2 () 25)', source)
            self.assertIn('(spawn 300 engine-loop)', source)
            self.assertIn('(if (= id 0x4EAu32) (handle-engine-command data))', source)
            self.assertIn('\n(def engine-enabled false)\n', source)
            self.assertIsNone(re.search(r'^\(def engine-enabled true\)$', source, re.M))
            for name in builder.PROTECTED_FUNCTIONS:
                self.assertEqual(builder.warp.extract_defun(original, name), builder.warp.extract_defun(source, name))
            for tag, data in imports.items():
                self.assertEqual(data, (backup / ('installed-' + tag + '.bin')).read_bytes() + b'\0')
            self.assertEqual(metadata['app_version'], int(version))
            self.assertEqual(output.stat().st_mode & 0o777, 0o700)

    def test_existing_engine_or_missing_hook_requires_review(self):
        with self.assertRaisesRegex(ValueError, 'already contains engine sound'):
            builder.add_engine((ROOT / 'app.lisp').read_text())
        source = self.legacy_source().replace('(spawn 200 decay-loop)', '(spawn 250 decay-loop)')
        with self.assertRaisesRegex(ValueError, 'expected one source hook'):
            builder.add_engine(source)

    def test_explicit_upgrade_replaces_engine_defaults_without_duplicating_hooks(self):
        canonical = (ROOT / 'app.lisp').read_text()
        source = re.sub(r'\(def sw-app-version \d+\)', '(def sw-app-version 3)', canonical)
        source = source.replace('(def engine-style 0)', '(def engine-style 2)')
        source = re.sub(r'^\(def engine-rpm [^\n]+\n', '', source, count=1, flags=re.M)
        result = builder.add_engine(source, upgrade=True)
        self.assertIn('(def engine-style 0)', result)
        self.assertEqual(len(re.findall(r'^\(def engine-style ', result, re.M)), 1)
        self.assertIn(re.search(r'^\(def engine-rpm [^\n]+', canonical, re.M)[0], result)
        self.assertIn(re.search(r'\(def sw-app-version \d+\)', canonical)[0], result)
        for hook in ('(spawn 300 engine-loop)', '(def dataArray_0x7ED (array-create 4))',
                     '(if (= id 0x4EAu32) (handle-engine-command data))'):
            self.assertEqual(result.count(hook), 1, hook)
        for name in builder.PROTECTED_FUNCTIONS:
            self.assertEqual(builder.warp.extract_defun(source, name), builder.warp.extract_defun(result, name))
        with self.assertRaisesRegex(ValueError, 'requires an existing'):
            builder.add_engine(self.legacy_source(), upgrade=True)
        with self.assertRaisesRegex(ValueError, 'existing engine hook'):
            builder.add_engine(source.replace('(spawn 300 engine-loop)', '(spawn 350 engine-loop)'), upgrade=True)

    def test_v4_upgrade_migrates_music_protocol_and_runs_midi(self):
        canonical = (ROOT / 'app.lisp').read_text()
        source = re.sub(r'^\(def music-(?:start-count|end-reason) [^\n]+\n', '', canonical, flags=re.M)
        source = source.replace('(def dataArray_0x7E6 (array-create 5))',
                                '(def dataArray_0x7E6 (array-create 3))')
        source = builder.warp.replace_defun(source, 'send-music-status', '(defun send-music-status () nil)')
        source = source.replace('(if (or music-playing music-starting) {\n            (stop-music)\n            (send-music-status)\n        })', '(if (or music-playing music-starting) (stop-music))')
        result = builder.add_engine(source, upgrade=True)
        for name in ('music-start-count', 'music-end-reason'):
            self.assertEqual(len(re.findall(r'^\(def ' + name + r' 0\)', result, re.M)), 1)
        self.assertIn('(def dataArray_0x7E6 (array-create 5))', result)
        self.assertEqual(builder.warp.extract_defun(result, 'send-music-status'),
                         builder.warp.extract_defun(canonical, 'send-music-status'))
        self.assertIn('(send-music-status)', builder.warp.extract_defun(result, 'handle-upload-begin'))
        self.assertEqual(result, builder.add_engine(result, upgrade=True))
        repl = os.environ.get('LBM_REPL', 'lbm')
        if not shutil.which(repl):
            self.skipTest('set LBM_REPL to execute the generated app')
        midi = builder.load(ROOT / 'tests/test_midi_lifecycle.py', 'upgrade_midi_tests')
        prelude = re.sub(r'^\(def music-(?:start-count|end-reason) [^\n]+\n', '', midi.PRELUDE, flags=re.M)
        prelude = prelude.replace('(defun send-music-status () { (def song-status-events (+ song-status-events 1)) true })', '')
        names = builder.MUSIC_STATE | set(builder.MUSIC_FUNCTIONS) | {'vscale', 'engine-pause'}
        selected = []
        for form in midi.forms(result):
            match = re.match(r'\((?:def|defun)\s+([^\s()]+)', form)
            if match and match[1] in names and match[1] != 'engine-pause':
                selected.append(form.replace('(ext-midi-open ', '(mock-midi-open ').replace(
                    '(ext-midi-parse parser-idx)', '(mock-midi-parse parser-idx generation)'))
        buffer = re.search(r'\(def dataArray_0x7E6 \(array-create \d+\)\)', result)[0]
        checks = r'''
(defun can-send-sid (id data) {
    (expect (and (= id 0x7E6) (= (buflen data) 5)) "five byte status")
    true
})
(play-current-song)
(expect (and music-playing (not music-starting) (= music-start-count 1)) "upgraded MIDI starts")
(send-music-status)
(expect (= (bufget-u8 dataArray_0x7E6 3) 1) "start counter on wire")
(sleep 0.12)
(expect (and (not music-playing) (= music-end-reason 2)) "upgraded MIDI finishes")
(expect (= (bufget-u8 dataArray_0x7E6 4) 2) "finish reason on wire")
(play-current-song)
(stop-music)
(send-music-status)
(expect (= (bufget-u8 dataArray_0x7E6 4) 1) "stop reason on wire")
(if (= failures 0) (print "UPGRADE_PASS") (print "UPGRADE_FAIL"))
'''
        with tempfile.NamedTemporaryFile('w', suffix='.lbm') as script:
            script.write(prelude + '\n' + buffer + '\n' + '\n'.join(selected) + '\n' + checks)
            script.flush()
            run = subprocess.run([repl, '--silent', '--history_file=', '--heap_size=16384',
                                  '--memory_size=32768', '--src', script.name, '--terminate'],
                                 capture_output=True, text=True, timeout=30)
        self.assertEqual(run.returncode, 0, run.stdout + run.stderr)
        self.assertIn('UPGRADE_PASS', run.stdout, run.stdout + run.stderr)
        self.assertNotIn('"FAIL"', run.stdout)

    def test_corrupt_resource_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            backup, _ = self.inputs(root)
            (backup / 'installed-midi.bin').write_bytes(b'changed')
            with self.assertRaisesRegex(ValueError, 'backup import mismatch'):
                builder.build(backup, root / 'output')

    def test_backport_does_not_add_unrelated_boost(self):
        source = self.legacy_source()
        source = re.sub(r'^\(def boost-[A-Za-z0-9_-]+ .+\)\n', '', source, flags=re.M)
        result = builder.add_engine(source)
        self.assertNotIn('(def boost-native-enabled', result)
        self.assertNotIn('(def boost-policy-enabled', result)


if __name__ == '__main__':
    unittest.main()
