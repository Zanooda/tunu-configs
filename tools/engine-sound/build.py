#!/usr/bin/env python3
"""Add engine sound to a target backup without replacing its riding logic or imports."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import shutil

REPO = Path(__file__).resolve().parents[2]
MUSIC_STATE = {'music-play-generation', 'music-active-tracks', 'music-starting', 'music-upload-last',
               'music-start-count', 'music-end-reason'}
MUSIC_FUNCTIONS = ('advance-music-generation', 'music-generation-current', 'music-track-finished',
                   'play-midi-track', 'stop-music', 'play-current-song', 'play-axelf', 'note-on', 'note-off',
                   'send-music-status')
PROTECTED_FUNCTIONS = ('check-shutdown', 'send-status2', 'send_stats', 'kers-fade-target',
                       'update-kers-fade', 'kers-voltage-scale', 'update-kers-current')


def load(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


warp = load(REPO / 'tools/warp-sound/build_private.py', 'engine_warp_builder')
flash = load(REPO / 'tunu-flash.py', 'engine_flash_builder')


def replace_once(source, before, after):
    if source.count(before) != 1:
        raise ValueError('expected one source hook: ' + before)
    return source.replace(before, after)


def add_engine(source, canonical=None, upgrade=False):
    canonical = canonical if canonical is not None else (REPO / 'app.lisp').read_text()
    has_engine = bool(re.search(r'^\(defun engine-step\b', source, re.M))
    if has_engine and not upgrade:
        raise ValueError('source already contains engine sound; use the explicit upgrade option after review')
    if upgrade and not has_engine:
        raise ValueError('engine upgrade requires an existing engine implementation')
    if upgrade:
        for hook in ('(engine-init)', '(spawn 300 engine-loop)', '(def dataArray_0x7ED (array-create 4))',
                     '(if (= id 0x4EAu32) (handle-engine-command data))'):
            if source.count(hook) != 1:
                raise ValueError('expected one existing engine hook: ' + hook)
    protected = {name: warp.extract_defun(source, name) for name in PROTECTED_FUNCTIONS}
    before_imports = flash.source_imports(source.encode())
    states = []
    for line in canonical.splitlines():
        match = re.fullmatch(r'\(def ([A-Za-z0-9_-]+) .+\)(?:\s*;[^\n]*)?', line)
        if match and (match[1].startswith('engine-') or match[1] in MUSIC_STATE):
            existing = re.findall(r'^\(def ' + re.escape(match[1]) + r'\s.+\)(?:[ \t]*;[^\n]*)?$', source, re.M)
            if len(existing) > 1:
                raise ValueError('duplicate state: ' + match[1])
            if existing and upgrade and match[1].startswith('engine-'):
                source = re.sub(r'^' + re.escape(existing[0]) + r'$', lambda _: line, source, count=1, flags=re.M)
            elif not existing:
                states.append(line)
    if not states and not has_engine:
        raise ValueError('canonical engine state is missing')
    if states:
        source = replace_once(source, '\n(def music-t-start (systime))\n',
                              '\n(def music-t-start (systime))\n' + '\n'.join(states) + '\n')
    functions = re.findall(r'^\(defun ((?:engine-[A-Za-z0-9_-]+)|handle-engine-command|send-engine-status(?:-locked)?)\s', canonical, re.M)
    if 'engine-step' not in functions or 'engine-loop' not in functions:
        raise ValueError('canonical engine functions are missing')
    additions = []
    for name in (*functions, *MUSIC_FUNCTIONS):
        definition = warp.extract_defun(canonical, name)
        if re.search(r'^\(defun ' + re.escape(name) + r'\s', source, re.M):
            source = warp.replace_defun(source, name, definition)
        else:
            additions.append(definition)
    if additions:
        source = replace_once(source, '; ===== Music: polyphony engine =====',
                              '\n\n'.join(additions) + '\n\n; ===== Music: polyphony engine =====')
    if not has_engine:
        for name in ('handle-upload-begin', 'handle-upload-chunk', 'handle-upload-end'):
            definition = warp.extract_defun(source, name)
            opening = '(defun ' + name + ' (data) {'
            update = opening + '\n    (def music-upload-last (systime))'
            if name == 'handle-upload-begin':
                update += '\n    (engine-pause)'
            source = warp.replace_defun(source, name, replace_once(definition, opening, update))
        source = replace_once(source, '    ; Music_Control_ID2 (0x4E9) - master volume',
                              '    (if (= id 0x4EAu32) (handle-engine-command data))\n\n'
                              '    ; Music_Control_ID2 (0x4E9) - master volume')
        source = replace_once(source, '(send-music-status)\n    })\n\n    ; Control_Message_ID1',
                              '(send-music-status)\n        (send-engine-status)\n    })\n\n    ; Control_Message_ID1')
        source = replace_once(source, '    (def dataArray_0x7EC (array-create 1))\n',
                              '    (def dataArray_0x7EC (array-create 1))\n    (def dataArray_0x7ED (array-create 4))\n')
        source = replace_once(source, '    (ext-midi-init 2) ; 2 simultaneous MIDI parsers = max 2 tracks per song\n',
                              '    (ext-midi-init 2) ; 2 simultaneous MIDI parsers = max 2 tracks per song\n    (engine-init)\n')
        source = replace_once(source, '    (spawn 200 decay-loop)\n',
                              '    (spawn 200 decay-loop)\n    (spawn 300 engine-loop)\n')
        source = replace_once(source, '(list abs loopforeach looprange loopwhile)',
                              '(list abs sin loopforeach looprange loopwhile)')
    source, buffers = re.subn(r'\(def dataArray_0x7E6 \(array-create (?:3|5)\)\)',
                              '(def dataArray_0x7E6 (array-create 5))', source)
    if buffers != 1:
        raise ValueError('expected one music status buffer')
    upload = warp.extract_defun(source, 'handle-upload-begin')
    stop_hook = '(if (or music-playing music-starting) (stop-music))'
    status_hook = '(if (or music-playing music-starting) {\n            (stop-music)\n            (send-music-status)\n        })'
    if stop_hook in upload:
        source = warp.replace_defun(source, 'handle-upload-begin',
                                    replace_once(upload, stop_hook, status_hook))
    elif status_hook not in upload:
        raise ValueError('unrecognized upload stop/status hook')
    app_version = re.search(r'^\(def sw-app-version ([0-9]+)\)$', canonical, re.M)
    if not app_version:
        raise ValueError('canonical app version is missing')
    source, versions = re.subn(r'^\(def sw-app-version [0-9]+\)$', app_version[0], source, flags=re.M)
    if versions != 1:
        raise ValueError('expected one app version')
    if '(def warp-samples-enabled true)' in source:
        for name in ('warp-finish-if-current', 'play-warp-samples'):
            source = warp.replace_defun(source, name, warp.extract_defun(warp.WARP_FUNCTIONS, name))
    for name, definition in protected.items():
        if warp.extract_defun(source, name) != definition:
            raise ValueError('protected target function changed: ' + name)
    if flash.source_imports(source.encode()) != before_imports:
        raise ValueError('target imports changed')
    return source


def build(backup, output, upgrade=False):
    backup = warp.require_private_path(backup, 'target backup')
    output = warp.prepare_private_output(output)
    original = (backup / 'before.lisp').read_text()
    source = add_engine(original, upgrade=upgrade)
    metadata = json.loads((backup / 'installed-imports.json').read_text())
    wanted = {tag for _, tag in flash.source_imports(source.encode())}
    overrides = {}
    for item in metadata:
        tag = item['tag']
        if tag not in wanted or tag in overrides:
            raise ValueError('unexpected or duplicate backup import: ' + tag)
        path = backup / ('installed-' + tag + '.bin')
        data = path.read_bytes()
        if len(data) + 1 != item['length'] or hashlib.sha256(data).hexdigest() != item['sha256']:
            raise ValueError('backup import mismatch: ' + tag)
        overrides[tag] = path
    if wanted != overrides.keys():
        raise ValueError('missing target backup imports')
    image, _ = flash.build_image(source.encode(), overrides=overrides)
    packed, imports = flash.unpack_blob(flash.validate_image(image))
    if packed != source.encode() or any(imports[tag] != path.read_bytes() + b'\0' for tag, path in overrides.items()):
        raise ValueError('image readback differs from target resources')
    for path, tag in flash.source_imports(source.encode()):
        if not path.startswith(('pkg', '://')):
            destination = (output / path).resolve()
            if output not in destination.parents or destination.name in ('app.lisp', 'engine.img', 'build.json'):
                raise ValueError('import path escapes/collides with output: ' + path)
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(overrides[tag], destination)
    (output / 'app.lisp').write_text(source)
    (output / 'engine.img').write_bytes(image)
    manifest = {'image_bytes': len(image), 'image_sha256': hashlib.sha256(image).hexdigest(),
                'source_sha256': hashlib.sha256(source.encode()).hexdigest(),
                'before_source_sha256': hashlib.sha256(original.encode()).hexdigest(),
                'imports': metadata, 'app_version': int(re.search(r'^\(def sw-app-version ([0-9]+)\)$', source, re.M)[1]),
                'protected_functions': list(PROTECTED_FUNCTIONS)}
    (output / 'build.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print('engine image: %d/%d bytes; %d target imports preserved' % (len(image), flash.MAX_IMAGE_SIZE, len(imports)))
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--backup-dir', type=Path, required=True)
    parser.add_argument('--output-dir', type=Path, required=True)
    parser.add_argument('--upgrade-engine', action='store_true', help='explicitly replace reviewed existing engine functions and defaults')
    args = parser.parse_args()
    build(args.backup_dir, args.output_dir, upgrade=args.upgrade_engine)


if __name__ == '__main__':
    main()
