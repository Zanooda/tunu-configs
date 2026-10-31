#!/usr/bin/env python3
"""Build an opt-in, sample-enabled app and staged music catalogue offline."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import shutil
import struct
import wave

REPO = Path(__file__).resolve().parents[2]
SONG_ID = 240
PCM_MIN_ID = 240
PCM_MAX_ID = 255
ENTRY = {"number": SONG_ID, "name": "Space Whoosh", "source": "builtin-samples"}
SAMPLE_BYTES = 44160
CHUNK_BYTES = 4000
IMPORTS = "\n".join(
    '(import "samples/warp-sample-%d.s8" \'warp-sample-%d)' % (i, i)
    for i in range(12)
)
CONFIG = """(def warp-samples-enabled true)
(def warp-song-number %d)
(def warp-sample-rate 16000.0)
(def warp-sample-voltage 0.15)
(def warp-sample-guard 0.02)
(def warp-max-scheduler-lateness 0.10)""" % SONG_ID
INIT = """(def warp-play-buffer-a (array-create 4000))
    (def warp-play-buffer-b (array-create 4000))
    (def warp-play-buffer-tail (array-create 160))
    (def warp-sample-chunks (list warp-sample-0 warp-sample-1 warp-sample-2
                                  warp-sample-3 warp-sample-4 warp-sample-5
                                  warp-sample-6 warp-sample-7 warp-sample-8
                                  warp-sample-9 warp-sample-10 warp-sample-11))"""
DISPATCH = "((and warp-samples-enabled (= cmd warp-song-number)) (play-warp-samples))"

WARP_FUNCTIONS = r'''(defun warp-generation-current (generation)
    (atomic (= generation music-play-generation))
)

(defun warp-queue-initial (generation) {
    (atomic (if (= generation music-play-generation) {
        (var first-count (- (buflen warp-sample-0) 1))
        (var second-count (- (buflen warp-sample-1) 1))
        (bufcpy warp-play-buffer-a 0 warp-sample-0 0 first-count)
        (foc-play-samples warp-play-buffer-a warp-sample-rate
                          (* warp-sample-voltage music-master-vol))
        (bufcpy warp-play-buffer-b 0 warp-sample-1 0 second-count)
        (foc-play-samples warp-play-buffer-b warp-sample-rate
                          (* warp-sample-voltage music-master-vol))
        true
    } false))
})

(defun warp-queue-chunk (generation destination source sample-count) {
    (atomic (if (= generation music-play-generation) {
        (bufcpy destination 0 source 0 sample-count)
        (foc-play-samples destination warp-sample-rate
                          (* warp-sample-voltage music-master-vol))
        true
    } false))
})

; Two slots are prefilled; 20ms guard and 100ms late limit preserve lookahead.
(defun warp-wait-until (generation start-time deadline) {
    (var remaining (- deadline (secs-since start-time)))
    (if (and (> remaining 0.0) (warp-generation-current generation))
        (sleep remaining))
    (and (warp-generation-current generation)
         (<= (- (secs-since start-time) deadline) warp-max-scheduler-lateness))
})

(defun warp-finish-if-current (generation) {
    (atomic (if (= generation music-play-generation) {
        (def music-play-generation (+ music-play-generation 1))
        (def current-song 0)
        (def music-active-tracks 0)
        (def music-starting false)
        (foc-play-stop)
        (loopforeach ch channels { (setix ch 1 0) })
        (def music-playing false)
        (send-music-status)
        true
    } false))
})

(defun play-warp-samples-track (generation) {
    (var start-time (systime))
    (var queued-samples (+ (- (buflen warp-sample-0) 1)
                           (- (buflen warp-sample-1) 1)))
    (var previous-count (- (buflen warp-sample-1) 1))
    (var sample-index 0)
    (var sequence-ok (warp-queue-initial generation))
    (loopforeach source warp-sample-chunks {
        (if (>= sample-index 2) {
            (var sample-count (- (buflen source) 1))
            (var deadline (+ (/ (- queued-samples previous-count) warp-sample-rate)
                             warp-sample-guard))
            (if (not (warp-wait-until generation start-time deadline))
                (setq sequence-ok false))
            (if (and sequence-ok (warp-generation-current generation)) {
                (var destination (if (= sample-index 11) warp-play-buffer-tail
                                     (if (= (mod sample-index 2) 0)
                                         warp-play-buffer-a warp-play-buffer-b)))
                (if (warp-queue-chunk generation destination source sample-count) {
                    (setq queued-samples (+ queued-samples sample-count))
                    (setq previous-count sample-count)
                } {
                    (setq sequence-ok false)
                })
            } {
                (setq sequence-ok false)
            })
        })
        (setq sample-index (+ sample-index 1))
    })
    (if (and sequence-ok (= sample-index 12)
             (warp-wait-until generation start-time
                              (+ (/ queued-samples warp-sample-rate)
                                 warp-sample-guard))) {
        (warp-finish-if-current generation)
    } {
        ; A late refill or cancellation must not let this task stop a newer play.
        (warp-finish-if-current generation)
    })
})

(defun play-warp-samples () {
    (var generation (advance-music-generation))
    (atomic {
        (def music-starting true)
        (def current-song 0)
        (def music-playing false)
        (def music-active-tracks 0)
        (engine-pause)
        (foc-play-stop)
        (loopforeach ch channels { (setix ch 1 0) })
    })
    (sleep 0.05)
    (atomic (if (= generation music-play-generation) {
        (def music-t-start (systime))
        (def current-song warp-song-number)
        (def music-playing true)
        (def music-starting false)
        (spawn 200 play-warp-samples-track generation)
    }))
})'''

MARKERS = {
    "; @private-warp-imports": IMPORTS,
    "; @private-warp-config": CONFIG,
    "; @private-warp-functions": WARP_FUNCTIONS,
    "; @private-warp-init": INIT,
    "; @private-warp-dispatch": DISPATCH,
}


def defun_span(source, name):
    matches = list(re.finditer(r"\(defun\s+" + re.escape(name) + r"(?=\s)", source))
    if len(matches) != 1:
        raise ValueError("expected one definition of " + name)
    start = matches[0].start()
    depth = 0
    in_string = escaped = in_comment = False
    for offset in range(start, len(source)):
        char = source[offset]
        if in_comment:
            if char == "\n":
                in_comment = False
            continue
        if in_string:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                in_string = False
            continue
        if char == '"':
            in_string = True
        elif char == ";":
            in_comment = True
        elif char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                return start, offset + 1
    raise ValueError("unterminated definition of " + name)


def extract_defun(source, name):
    start, end = defun_span(source, name)
    return source[start:end]


def replace_defun(source, name, replacement):
    start, end = defun_span(source, name)
    return source[:start] + replacement + source[end:]


PRIVATE_FUNCTIONS = {name: extract_defun((REPO / 'app.lisp').read_text(), name)
                     for name in ('stop-music', 'play-current-song', 'play-axelf')}


def injected_app(source, song_number=SONG_ID):
    for name, snippet in PRIVATE_FUNCTIONS.items():
        if extract_defun(source, name) != snippet:
            raise ValueError("target music function differs from reviewed source: " + name)
    if not PCM_MIN_ID <= song_number <= PCM_MAX_ID:
        raise ValueError("sample song number must be in the range 240-255")
    for marker, snippet in MARKERS.items():
        if marker == "; @private-warp-config":
            snippet = snippet.replace("(def warp-song-number %d)" % SONG_ID,
                                      "(def warp-song-number %d)" % song_number)
        if source.count(marker) != 1:
            raise ValueError("expected exactly one app injection point: " + marker)
        source = source.replace(marker, snippet)
    return source


def load_module(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def require_private_path(path, label):
    path = Path(path).resolve()
    if path == REPO or REPO in path.parents:
        raise ValueError(label + " must stay outside the repository")
    return path


def prepare_private_output(path):
    output = require_private_path(path, "private generated bundle")
    if output.exists():
        if not output.is_dir():
            raise ValueError("private output path must be a directory")
        if output.stat().st_mode & 0o077:
            raise ValueError(
                "existing private output directory must not be accessible by group or others")
        return output

    missing = []
    parent = output
    while not parent.exists():
        missing.append(parent)
        parent = parent.parent
    if not parent.is_dir():
        raise ValueError("private output parent must be a directory")
    for directory in reversed(missing):
        directory.mkdir(mode=0o700)
        directory.chmod(0o700)
    return output


def stock_resource_overrides(backup, source=None, flash=None):
    metadata = json.loads((backup / "installed-imports.json").read_text())
    if source is None:
        wanted = {"midi", "song1-track0", "song1-track1"}
        metadata = [entry for entry in metadata if entry["tag"] in wanted]
    else:
        wanted = {tag for _, tag in flash.source_imports(source.encode())}
    overrides = {}
    for entry in metadata:
        tag = entry["tag"]
        if tag not in wanted or tag in overrides:
            raise ValueError("unexpected or duplicate preserved resource: " + tag)
        path = backup / ("installed-" + tag + ".bin")
        data = path.read_bytes()
        if entry["length"] != len(data) + 1:
            raise ValueError("preserved resource length mismatch: " + tag)
        if hashlib.sha256(data).hexdigest() != entry["sha256"]:
            raise ValueError("preserved resource hash mismatch: " + tag)
        overrides[tag] = path
    if overrides.keys() != wanted:
        raise ValueError("missing preserved resources: " + ", ".join(sorted(wanted - overrides.keys())))
    return overrides


def verify_image(flash, image, source, expected_imports):
    blob = flash.validate_image(image)
    packed_source, imports = flash.unpack_blob(blob)
    if packed_source != source:
        raise ValueError("packed source differs from generated application")
    if imports.keys() != expected_imports.keys():
        raise ValueError("unexpected import table tags")
    for tag, data in expected_imports.items():
        if imports[tag] != data + b"\x00":
            raise ValueError("imported resource mismatch: " + tag)
    return len(image)


def build_image(flash, source, base_dir, stock_overrides, samples=None):
    overrides = dict(stock_overrides)
    expected = {tag: path.read_bytes() for tag, path in stock_overrides.items()}
    if samples:
        for index, path in enumerate(samples):
            tag = "warp-sample-%d" % index
            overrides[tag] = path
            expected[tag] = path.read_bytes()
    image, _ = flash.build_image(source, base_dir=base_dir, overrides=overrides)
    verify_image(flash, image, source, expected)
    return image


def stage_catalog(base_catalog, song_number=SONG_ID, migrate_legacy=False):
    if not isinstance(base_catalog, list) or any(not isinstance(item, dict) for item in base_catalog):
        raise ValueError("base catalogue must be an array of song objects")
    if not PCM_MIN_ID <= song_number <= PCM_MAX_ID:
        raise ValueError("sample song number must be in the range 240-255")
    matches = [item for item in base_catalog if item.get("number") == song_number]
    if len(matches) > 1:
        raise ValueError("song number %d appears more than once" % song_number)
    legacy = [item for item in base_catalog if item.get("number") == 7
              and item.get("source") == "builtin-samples" and not item.get("tracks")
              and (item.get("default") == "tng-warp2" or item.get("name") in ("TNG Warp 2", "Space Whoosh"))]
    if legacy:
        if not migrate_legacy:
            raise ValueError("legacy sample at 7: use --migrate-legacy only when updating the controller and trigger together")
        if matches:
            raise ValueError("cannot migrate legacy sample: song number %d is already in use" % song_number)
        return [dict(item, number=song_number) if item is legacy[0] else item for item in base_catalog]
    if matches:
        existing = matches[0]
        if existing.get("source") != "builtin-samples" or existing.get("tracks"):
            raise ValueError("song number %d is not a resident sample entry" % song_number)
        return list(base_catalog)
    if ENTRY["name"] in [item.get("name") for item in base_catalog]:
        raise ValueError("song name already exists in the base catalogue")
    return list(base_catalog) + [dict(ENTRY, number=song_number)]


def upgrade_existing_app(source, song_number):
    if not PCM_MIN_ID <= song_number <= PCM_MAX_ID:
        raise ValueError("sample song number must be in the range 240-255")
    if source.count("(def warp-samples-enabled true)") != 1:
        raise ValueError("expected an enabled resident sample app")
    for name in ("play-warp-samples", "play-warp-samples-track", "warp-finish-if-current"):
        extract_defun(source, name)
    source, count = re.subn(r"^\(def warp-song-number (?:7|24[0-9]|25[0-5])\)$",
                            "(def warp-song-number %d)" % song_number, source, flags=re.M)
    if count != 1:
        raise ValueError("expected one supported existing sample song number")
    upload = extract_defun(source, "handle-upload-begin")
    old = "(if music-playing (stop-music))"
    fixed = "(if (or music-playing music-starting) (stop-music))"
    if old in upload:
        if upload.count(old) != 1:
            raise ValueError("ambiguous upload stop hook")
        source = replace_defun(source, "handle-upload-begin", upload.replace(old, fixed))
    elif fixed not in upload and not re.search(
            r'\(if\s+\(or music-playing music-starting\)\s*\{\s*'
            r'\(stop-music\)\s*\(send-music-status\)\s*\}\)', upload):
        raise ValueError("unrecognized upload stop hook")
    return source


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--source-audio", type=Path, help="private audio file converted with ffmpeg")
    source.add_argument("--sample-stream", type=Path, help="private signed 8-bit 16 kHz stream")
    parser.add_argument("--sample-sha256", help="expected converted sample-stream SHA256")
    parser.add_argument("--resource-backup", type=Path, required=True,
                        help="directory with installed-imports.json and preserved package resources")
    parser.add_argument("--app-source", type=Path, default=REPO / "app.lisp",
                        help="target Lisp source with PCM hooks (default: repository app.lisp)")
    parser.add_argument("--song-number", type=int, default=SONG_ID,
                        help="resident sample number, 240-255 (default: 240)")
    parser.add_argument("--upgrade-existing-pcm", action="store_true",
                        help="update a backed-up PCM app while preserving its target-specific source")
    parser.add_argument("--migrate-legacy", action="store_true",
                        help="move a recognized legacy sample entry at 7 in the staged catalogue")
    parser.add_argument("--base-catalog", type=Path, required=True,
                        help="existing songs.json to preserve and stage the sample entry")
    parser.add_argument("--output-dir", type=Path, required=True,
                        help="private output directory outside the repository")
    return parser.parse_args()


def main():
    args = parse_args()
    output = prepare_private_output(args.output_dir)
    if args.resource_backup:
        args.resource_backup = require_private_path(args.resource_backup, "captured VESC resources")
    if args.source_audio:
        args.source_audio = require_private_path(args.source_audio, "source audio")
    if args.sample_stream:
        args.sample_stream = require_private_path(args.sample_stream, "sample stream")
    base_catalog = json.loads(args.base_catalog.read_text())
    catalog = stage_catalog(base_catalog, args.song_number, args.migrate_legacy)

    if args.source_audio:
        converter = load_module(Path(__file__).with_name("convert_audio.py"), "warp_convert_audio")
        sample_path = output / "warp-sound-16000.s8"
        converter.convert_audio(args.source_audio, sample_path, args.sample_sha256)
        sample = sample_path.read_bytes()
    else:
        sample = args.sample_stream.read_bytes()
    if len(sample) != SAMPLE_BYTES:
        raise ValueError("sample must contain exactly %d signed 16 kHz mono samples" % SAMPLE_BYTES)
    sample_digest = hashlib.sha256(sample).hexdigest()
    if args.sample_sha256 and sample_digest != args.sample_sha256:
        raise ValueError("sample stream SHA256 does not match the expected digest")

    sample_dir = output / "bundle/vesc/samples"
    sample_dir.mkdir(parents=True, exist_ok=True)
    chunks = []
    for index, offset in enumerate(range(0, len(sample), CHUNK_BYTES)):
        path = sample_dir / ("warp-sample-%d.s8" % index)
        path.write_bytes(sample[offset:offset + CHUNK_BYTES])
        chunks.append(path)
    if len(chunks) != 12 or b"".join(path.read_bytes() for path in chunks) != sample:
        raise ValueError("sample chunks do not reconstruct the source stream")

    preview = output / "tng-warp-2-sampled-preview.wav"
    pcm = b"".join(struct.pack("<h", (value if value < 128 else value - 256) * 256)
                   for value in sample)
    with wave.open(str(preview), "wb") as target:
        target.setnchannels(1)
        target.setsampwidth(2)
        target.setframerate(16000)
        target.writeframes(pcm)

    source_path = args.app_source.resolve()
    default_source = source_path.read_text()
    flash = load_module(REPO / "tunu-flash.py", "tunu_flash")
    stock = stock_resource_overrides(args.resource_backup.resolve(), default_source, flash)
    default_image = build_image(flash, default_source.encode(), source_path.parent, stock)
    if args.upgrade_existing_pcm:
        expected_tags = {"warp-sample-%d" % i for i in range(12)}
        if not expected_tags.issubset(stock):
            raise ValueError("existing PCM app is missing sample imports")
        if b"".join(stock["warp-sample-%d" % i].read_bytes() for i in range(12)) != sample:
            raise ValueError("sample stream differs from target backup")
        private_source = upgrade_existing_app(default_source, args.song_number)
    else:
        if "warp-samples-enabled" in default_source or "warp-song-number" in default_source:
            raise ValueError("source already has PCM; use --upgrade-existing-pcm after review")
        if "samples/warp-sample-" in default_source or "@private-warp-dispatch" not in default_source:
            raise ValueError("source lacks unused PCM insertion points")
        private_source = injected_app(default_source, args.song_number)
    private_app = output / "bundle/vesc/app.lisp"
    private_app.write_text(private_source)
    image = build_image(flash, private_source.encode(), private_app.parent, stock, chunks)
    image_path = output / "bundle/vesc/warp-sample.img"
    image_path.write_bytes(image)

    stage = output / "bundle/music-service"
    stage.mkdir(parents=True, exist_ok=True)
    (stage / "songs.json").write_text(json.dumps(catalog, indent=2) + "\n")
    extension_dir = output / "bundle/extensions"
    extension_dir.mkdir(parents=True, exist_ok=True)
    trigger = (REPO / "extensions/warp-sound.py").read_text()
    old_number = "SONG_NUMBER = 240"
    if trigger.count(old_number) != 1:
        raise ValueError("expected one default trigger song number")
    (extension_dir / "warp-sound.py").write_text(trigger.replace(old_number, "SONG_NUMBER = %d" % args.song_number))
    shutil.copy2(REPO / "extensions/warp-sound.toml", extension_dir / "warp-sound.toml")
    limit = flash.MAX_IMAGE_SIZE
    if len(image) > limit:
        raise ValueError("private image exceeds VESC image capacity")
    print("default app image: %d bytes; private sample app image: %d/%d bytes" %
          (len(default_image), len(image), limit))
    print("staged sample song %d; preserved %d existing catalogue entries" %
          (args.song_number, len(base_catalog)))
    print("private image: %s sha256=%s" % (image_path, hashlib.sha256(image).hexdigest()))
    print("sample stream sha256=%s" % sample_digest)
    print("preview: %s" % preview)


if __name__ == "__main__":
    main()
