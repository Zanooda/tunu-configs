# Warp-sound trigger prototype

`warp-sound.toml` is deliberately disabled. It observes `engine-ecu.speed`; `warp-sound.py` requires a recent speed event whose `from` matches its own prior speed observation and whose prior observation is no more than one second old. It also checks the current speed and `vehicle.state`, drops concurrent work instead of queueing it, rearms at or below 45 km/h, and enforces a 45-second cooldown. Timestamps measure event-service observation emission age, not ECU acquisition age. The helper stores its baseline and latch under `tunu:warp-sound:trigger-state`, outside vehicle hashes.

The generic `app.lisp` keeps sample playback disabled and imports no private audio. Only the generated private app image injects the `foc-play-samples` player, sample imports and dispatch. New builds default to song 240; the staged trigger uses the same selected number. It sends no PCM over CAN. The waveform, generated app and staged bundle remain private and are not distributed here.

The helper invalidates its baseline and arming after stale, out-of-order, mismatched or invalid observations, so a crossing needs a recent matching predecessor observed by this helper. This bounds the observation chain but cannot identify a reconnect that occurs within the one-second window. Keep the rule disabled until deployment review confirms event continuity and target runtime behavior.

## Native handler for speed events

Use `warp-trigger/` for enabled, high-frequency speed rules. It implements the
same observation policy as the Python helper, without starting Python or
`redis-cli` for each event. Each invocation uses one Redis connection with a
200 ms connect timeout and a 400 ms total command deadline. It keeps the same
state key, 50 km/h crossing, 45 km/h rearm and 45-second cooldown.

```sh
cd extensions/warp-trigger
CGO_ENABLED=0 GOOS=linux GOARCH=arm GOARM=7 go build -trimpath -ldflags '-s -w' -o tunu-warp-trigger .
go test -race ./...
go vet ./...
```

Install the binary as `/data/extensions/bin/tunu-warp-trigger` and change the
existing rule's `command` to that path. Keep `concurrency = "drop"` and the
existing timeout; restart only `librescoot-events` after changing its rule.
Back up the rule first and verify it loads. The binary defaults to resident
song 240; a different prepared resident number can be selected with
`--song-number` in an executable wrapper. `--state-key` and `--music-channel`
allow isolated diagnostics without modifying the real latch or sending music
commands. The Python helper remains available as a policy reference and
fallback; do not run both handlers for the same input.

Offline tests:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -p 'test_warp_sound.py' -v
```

# Optional timed boost

[Timed boost](../tools/boost/README.md) uses an independently expiring native current guard and a disabled-by-default Lisp policy. The common peak is 85 A per eligible pack, up to 170 A aggregate. Native motor configurations remain unchanged. The optional `boost-supply.py` service requires fresh controller telemetry and changed pack observations; it never powers the ECU or activates a battery.

When enabling this feature, update `battery-current.py` and explicitly configure `boost_guard: true` so normal-current reconciliation goes through the guard instead of overwriting active boost. `single_current_a` can preserve an installation's normal single-pack limit; it defaults to the existing 80 A. The supply service is separate from event-service workers and must be installed/enabled explicitly with the matching controller app and ECU-service decoder.

# Battery-current events

These rules reconcile TUNU runtime limits when engine power turns on or a battery is inserted, removed or changes state. They require Librescoot event-service and Python 3 on the MDB.

The policy selects **150 A total battery current by default**, configurable down to 80 A, only when both packs are present and active; otherwise it selects **80 A**. These are installation-specific limits, not a hardware rating. Battery admission remains battery-service's responsibility. This extension neither activates a pack nor changes engine power.

The helper uses CAN sender **251**, separate from ECU-service's 252 and the uploader's default 253. Install the current `tunu-flash.py`: its receive filter excludes broadcast traffic, and its REPL pacing respects the firmware's command interval.

After power-on, the helper polls with read-only Lisp queries until the interpreter responds, within a 20-second budget. It then reads fresh battery state, changes only the requested runtime limits, verifies tagged readback, and reconciles again if the active pack count changed. Concurrent event rules share a lock. A failed event-service action ends the whole sequence, so rule-level `repeat` is not a retry mechanism; readiness retries belong in the helper.

## Installation

Install these files as root, with the directory writable only by root:

| Repository file | MDB destination |
| --- | --- |
| `extensions/battery-current.py` | `/data/extensions/battery-current.py`, executable |
| `extensions/battery-current.toml` | `/data/extensions/battery-current.toml` |
| `tunu-flash.py` | `/data/extensions/tunu-flash.py` |

Back up existing files first. Restart **only** `librescoot-events` after replacing the rules; it does not hot-reload TOML. No ECU, vehicle-service or battery-service restart is needed. Check that both rules loaded. A service restart does not replay an already-on engine state: run the helper once to reconcile the current state after installation.

```sh
lsc ext list
lsc ext test tunu-battery-current-power-on --event '{"topic":"vehicle.engine-power.changed","to":"on"}'
/data/extensions/battery-current.py
journalctl -t tunu-battery-current --no-pager -n 20
```

`lsc ext test` is a dry run. Running the helper can change live battery-current and explicitly configured power limits. It never writes configuration to flash. Confirm the intended limits before enabling it on a vehicle.

## Per-installation limits

Without `/data/extensions/battery-current.json`, the helper preserves the existing power ceiling and uses the default 150 A dual-pack ceiling. To explicitly select 7 kW and 120 A combined on an installation validated for it:

```json
{"power_max_w": 7000, "dual_current_a": 120}
```

Both fields are optional. `dual_current_a` accepts 80 to 150 A; single-pack operation remains limited to 80 A. This is a fixed ceiling, not a timed boost or continuous/peak-current policy.

The helper reapplies these limits after power-on and battery-state changes. Motor current, regenerative limits, thermal protection and motor calibration are unchanged. A power ceiling and battery-current ceiling both apply: at 55 V, 5 kW corresponds to about 91 A and 8 kW to about 145 A. The 80 A single-pack limit still bounds battery draw when an 8 kW power ceiling is configured.

The repository's `vesc_mcconf.xml` and `3kw-vesc_mcconf.xml` both specify 210 A motor current. Motor phase current is a separate limit from battery input current; changing the battery count alone does not require raising motor current.

## Tests

```sh
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s tests -p 'test_battery_current.py' -v
```

Tests cover startup readiness, bounded retries, engine-off handling, battery changes before and after writes, unchanged limits, optional power configuration and rejection of unrelated REPL output. They do not contact hardware.
