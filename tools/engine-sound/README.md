# Engine sound

Lisp application version 5 implements music-service's engine-sound protocol using VESC's existing four polyphonic audio channels. No controller firmware replacement or riding-limit change is needed. The controller starts silent on every boot; music-service owns the persistent mode and sends the requested state when appropriate.

The presets are procedural V8, moped, sport, V-twin and electric sounds. Pitch follows calibrated speed through virtual gears (3 to 6, depending on the style): within a gear the revs climb to the style's redline, an upshift drops them and briefly dips the level like a clutch. Holding the brake at a standstill lets the throttle rev the engine. Each style has its own start and stop sequence (starter with misfires, kick start, power-up chime; run-down, rattle, power-down), synthesized rather than recorded, and some have their own character on top (an uneven V-twin beat). The four tones of a style are scaled so every style has the same total level, 2.25 x the engine voltage; at full volume that is 4.5 V in total. The style parameters are constants in flash. This is an audio overlay, not a change to phase current, battery current, power, speed, field weakening or regen configuration.

> **Warning:** don't run the engine sound much above the default 50 % for long. Its low tones drive current through the motor windings, and all of it becomes heat, not torque; high volumes can physically damage the motor. Don't raise `engine-vol-max` either.

## Commands and priority

The existing standard CAN IDs are retained:

- `0x4EA`: command (0 off, 1 on, 2 demo), volume percent, optional style (0-4).
- `0x7ED`: requested on/off state, exact volume percent, demo-active flag, style.

State is echoed after commands and once per second, including during start, stop and demo sequences. All sequences run on the engine worker; a demo takes over an in-progress start or stop sequence without a second renderer writing the channels. Duplicate on/off/demo commands do not restart or extend a cue/demo. The demo lasts about 30 seconds, uses simulated inputs only, and leaves the persistent on/off request unchanged. An off command cancels a demo immediately. Start and stop sequences take 1 to 2 seconds.

MIDI, sample playback and pending song starts have priority. Upload traffic also reserves a short quiet interval. Engine audio never writes the shared channels while a foreground player owns them. Natural completion clears the music state only after every track in the current generation has finished; stale same-song tasks cannot parse, emit notes or finish a newer playback. This also lets music-service restore the saved music volume after an unlock melody.

If the engine worker exits with an error, it releases its audio channels and reports off. Native controller fault handling remains authoritative.

## music-service

Use the service version that includes `music engine` and the matching `0x4EA`/`0x7ED` protocol. The older v1.0.0 CLI cannot control this feature.

```sh
music engine on
music engine ride
music engine for 20m
music engine vol 50
music engine style v8
music engine demo
music engine off
```

Persistent mode `on` sounds only in `ready-to-drive`; returning to `parked` silences it. Songs take priority, including a configured ready-to-drive fanfare. A locked controller can be powered for maintenance without changing vehicle state, but the normal demo CLI deliberately treats that state as unpowered. Stationary maintenance validation therefore needs an explicitly guarded diagnostic path, not a forged vehicle state or an unlock.

When upgrading an existing installation, preserve its catalogue, tracks, unlock trigger and saved music volume explicitly. Missing `music.volume` otherwise takes the service's new default. Engine volume is separate from music volume.

## Preserve a target's existing app

Do not replace a custom target app with the generic app just to add engine sound. First obtain a fresh import-aware backup with `before.lisp`, `installed-imports.json` and the corresponding `installed-<tag>.bin` files. Then build offline:

```sh
python3 tools/engine-sound/build.py \
  --backup-dir /private/target-backup \
  --output-dir /private/engine-build
```

For an app that already has engine sound, explicitly pass `--upgrade-engine` after reviewing the backed-up source. That replaces the engine functions/defaults without duplicating command, upload or worker hooks.

The builder adds or upgrades the engine renderer, updates generation-aware music lifecycle functions and patches the required entry points. It verifies that the target's shutdown, temperature, speed/odometer reporting and KERS functions remain identical. Every import is retained byte-for-byte, including native MIDI and private samples. It does not add or enable boost. Output stays outside the repository in a private directory.

Before installation, validate the complete image, source and imports. Pin the controller identity, back up its current motor/app configuration and odometer, keep app output inhibited while stationary, perform complete warm and cold readbacks, and compare configuration while separately bounding expected sensor auto-zero changes. Disarm the alarm before an authorized sound test and restore its prior policy afterward; motor vibration can trigger motion detection. Never unlock or conduct a riding test without explicit authorization.

## Offline tests

```sh
LBM_REPL=/path/to/lispBM/repl/repl PYTHONDONTWRITEBYTECODE=1 \
  python3 -m unittest discover -s tests -p 'test_engine_*.py' -v
```

Tests execute the actual controller functions in host LispBM: commands and periodic status, start and stop sequences, malformed commands and volume clamping, silence during songs, song starts, uploads and faults, every style's demo within the voltage cap and frequency range, virtual gears, braked revving and worker-error cleanup. Simulated-clock tests interrupt startup with uploads, faults and errors; a native-clock test exercises demo handoff on the actual 300-cell worker stack. Set `ENGINE_APP_SOURCE` to a generated target app to test those same functions after backporting.
