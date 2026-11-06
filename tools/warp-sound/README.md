# Offline sampled warp-sound tooling

The repository contains reusable conversion and opt-in build tools, not a recording or captured VESC package binaries. Normal app builds and tests need none of the private inputs or FFmpeg. Keep supplied audio, converted samples, native resource backups, catalogues containing private entries, and generated bundles outside Git. No project-wide license or redistribution permission for a supplied recording is implied.

## Quick start

Run from the repository root on the MDB, with the ECU already safely powered, the vehicle stationary and locked, and OTA idle. Set `SAMPLE` to a signed 8-bit mono 16 kHz PCM file with exactly 44,160 bytes. The controller must already have compatible native MIDI imports.

```sh
SAMPLE=/path/to/sample.s8
WORK="/data/tunu-pcm-$(date +%s)"; mkdir -m 700 "$WORK"

python3 tunu-flash.py --no-engine read --backup-dir "$WORK/backup"

python3 tools/warp-sound/build_private.py \
  --app-source "$WORK/backup/before.lisp" \
  --resource-backup "$WORK/backup" \
  --sample-stream "$SAMPLE" \
  --base-catalog /data/music-service/songs.json \
  --output-dir "$WORK/build"

python3 tunu-flash.py --no-engine write \
  --image "$WORK/build/bundle/vesc/warp-sample.img" \
  --keep-output-disabled

cp -p /data/music-service/songs.json "$WORK/songs.before.json"
cp "$WORK/build/bundle/music-service/songs.json" /data/music-service/songs.json
systemctl restart music-service
```

The backup contains the original Lisp source, all imports and a rollback image. The builder rejects incompatible source hooks, changed shared music functions, missing resources and a conflicting sample song number; it does not overwrite the input source or catalogue. Review the generated source before uploading. Output inhibition expires after 120 seconds, so check controller faults, settings, odometer and a cold-boot readback before returning the vehicle to use. If rolling back, upload `$WORK/backup/rollback.img` and restore `$WORK/songs.before.json`. The event-trigger extension remains disabled until explicitly configured.

## Requirements

- Python 3 with its standard library.
- FFmpeg available on `PATH` for audio conversion.
- A local `tunu-configs` checkout and the matching `tunu-flash.py` image builder.
- For a complete image, a private resource-backup directory containing `installed-imports.json`, `installed-midi.bin`, `installed-song1-track0.bin`, and `installed-song1-track1.bin` from the target readback. These captured binaries are build inputs only and must not be copied into this repository.
- A base music-service `songs.json` to preserve.

## Prepare resources

For a target whose ECU is already powered safely, this reads the installed app over CAN and saves its Lisp source, complete blob, rollback image and individual imports in a new private directory:

```sh
python3 tunu-flash.py --no-engine read --backup-dir /private/target-backup
```

The saved source is `/private/target-backup/before.lisp`. The directory must not already exist. For a previously captured `read --blob` file, extract imports offline instead:

```sh
python3 tunu-flash.py extract /private/before.blob -o /private/resources
```

For a fresh build with local VESC Tool packages, resolve the generic source's imports without a scooter:

```sh
python3 tunu-flash.py extract app.lisp --source \
  --resource-root /path/to/resources -o /private/resources
```

The resource root must contain `vesc_packages/`. `--import TAG=FILE` can replace an individual package import with a raw local file. No packages are downloaded. A native MIDI library must match the target firmware.

## Build the sample image

Choose private paths for the audio file, resource backups, base catalogue, and output directory. The output directory must be outside this repository. A newly created output directory is restricted to mode `0700`; an existing directory with group/other access is rejected without changing it. Record `ffmpeg -version` alongside build results when exact reproduction matters.

Convert a supplied audio file to raw signed 8-bit, mono, 16 kHz PCM:

```sh
python3 tools/warp-sound/convert_audio.py \
  --input "$SOURCE_AUDIO" \
  --output "$PRIVATE_OUTPUT/warp-sound-16000.s8" \
  --expected-sha256 "$EXPECTED_SAMPLE_SHA256"
```

`--expected-sha256` is optional when first producing a new sample; the command prints the resulting digest. The fixed player format requires exactly 44,160 samples (2.760 seconds). Conversion uses FFmpeg's first audio stream, `-f s8 -ac 1 -ar 16000`, with no normalization filter. Supply a clip of the intended duration; the converter rejects other output lengths and does not truncate or pad.

Build the complete opt-in app and staged catalogue from that converted stream:

```sh
python3 tools/warp-sound/build_private.py \
  --sample-stream "$PRIVATE_OUTPUT/warp-sound-16000.s8" \
  --sample-sha256 "$EXPECTED_SAMPLE_SHA256" \
  --resource-backup "$RESOURCE_BACKUP" \
  --app-source "$RESOURCE_BACKUP/before.lisp" \
  --base-catalog "$BASE_CATALOG" \
  --output-dir "$PRIVATE_OUTPUT"
```

Alternatively, use `--source-audio "$SOURCE_AUDIO"` instead of `--sample-stream` to convert during the build. The expected digest is optional for the builder; when omitted, it prints the converted sample digest. `--sample-sha256` verifies a previously recorded digest.

The builder cross-compiles the native event trigger with Go 1.21 or newer.
Use `--go /path/to/go` when Go is not on PATH, or pass `--trigger-binary` with
an existing ARM32 build. The staged extension includes the native binary, an
executable wrapper carrying the selected song number, and the disabled rule.
It does not start Python for speed events. Install the wrapper and `bin/`
contents along with the rule; staging does not enable it.

The player defaults to song 240. Pass `--song-number` to select another reserved number from 240 to 255. The builder preserves an existing trackless `builtin-samples` entry at that number and refuses to overwrite any conflicting entry. A recognized legacy sample at song 7 requires the explicit `--migrate-legacy` option; migrate the controller image, staged catalogue and staged trigger together. Existing controller images continue using their previous number until updated. It injects sample imports/player/dispatch only into the generated app, separately builds the generic app without sample files, validates preserved resources against metadata, checks image imports including their NUL terminators, and enforces VESC image capacity. It writes the preview, sample chunks, app image/source, staged catalogue, and disabled event extension only to the selected private output directory. It never writes to a vehicle. `--app-source` uses the captured target source instead of the generic repository app. The source must contain all PCM insertion points and its shared music functions must match the reviewed repository implementation; otherwise the builder refuses to continue. All imports in the source must be present in the backup and match its recorded hashes. For an existing sampled app, use `--upgrade-existing-pcm --migrate-legacy` with a fresh target backup and the exact same sample stream. The builder checks the backed-up sample bytes, retains target imports and logic, changes the resident number and applies the merged upload-start cancellation fix. Review the staged controller image, catalogue and trigger as one migration. Incompatible custom music code needs separate review. Without `--app-source`, the builder uses the generic repository app; do not use that default to replace a customized target blindly.

## Offline checks

```sh
python3 tests/test_warp_audio.py
python3 tests/test_warp_private_builder.py
python3 tests/test_warp_samples.py
python3 tests/test_warp_samples.py --source "$PRIVATE_OUTPUT/bundle/vesc/app.lisp" --repl "$LBM_REPL"
python3 tests/test_warp_sound.py
```

The converter tests do not need FFmpeg or private media. The private LispBM test uses mocked VESC calls to exercise generation identity, replay/stop/switch races, bounded two-buffer scheduling, late-refill abort, and complete final-sample playback.

A successful offline build does not verify target firmware/API compatibility, the installed music-service binary, event reconnect continuity, playback safety, or redistribution rights. Keep the trigger disabled and do not install or play the image without separate review and approval.
