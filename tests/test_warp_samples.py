#!/usr/bin/env python3
"""Validate default sample opt-in, queue timing, and private LispBM playback."""
import argparse
import os
from pathlib import Path
import re
import subprocess
import tempfile

from test_drive_limits import forms

ROOT = Path(__file__).resolve().parents[1]
SAMPLE_IMPORT = re.compile(r"\(import \"samples/warp-sample-(1[01]|[0-9])\.s8\" 'warp-sample-\1\)")
SAMPLE_FUNCTIONS = {
    "advance-music-generation", "play-axelf", "play-current-song", "play-warp-samples",
    "play-warp-samples-track", "stop-music", "warp-finish-if-current",
    "warp-generation-current", "warp-queue-chunk", "warp-queue-initial",
    "warp-wait-until", "music-generation-current",
}
SAMPLE_CONFIG = {
    "warp-samples-enabled", "warp-song-number", "warp-sample-rate",
    "warp-sample-voltage", "warp-sample-guard", "warp-max-scheduler-lateness",
}


def simulate_queue(sample_counts, wake_lateness, sample_rate=16000.0,
                   guard=0.02, max_lateness=0.10):
    """Model a two-slot FIFO whose consumer advances at the sample clock."""
    durations = [count / sample_rate for count in sample_counts]
    now = 0.0
    cursor = 0.0
    pending = []
    starts = []
    for index, duration in enumerate(durations):
        if index < 2:
            call_at = now
        else:
            deadline = sum(durations[:index - 1]) + guard
            late = wake_lateness[index - 2]
            if late > max_lateness:
                return {"aborted": True, "starts": starts, "stop_at": deadline + late}
            call_at = deadline + late
        pending = [end for end in pending if end > call_at]
        if len(pending) >= 2:
            raise AssertionError("sample queue call would block on two occupied slots")
        start = max(cursor, call_at)
        starts.append(start)
        cursor = start + duration
        pending.append(cursor)
        now = call_at
    final_deadline = sum(durations) + guard
    finish_at = final_deadline + (wake_lateness[-1] if len(wake_lateness) > len(durations) - 2 else 0)
    return {"aborted": False, "starts": starts, "finish_at": finish_at,
            "audio_end": cursor}


def check_queue_model():
    counts = [4000] * 11 + [160]
    for lateness in (
        [0.0] * 11,
        [0.01, 0.025, 0.04, 0.055, 0.07, 0.085, 0.10, 0.08, 0.06, 0.04, 0.05],
    ):
        result = simulate_queue(counts, lateness)
        if result["aborted"]:
            raise AssertionError("bounded scheduler delay should keep the queue continuous")
        starts = result["starts"]
        if any(abs(starts[i] - sum(counts[:i]) / 16000.0) > 1e-9
               for i in range(1, len(starts))):
            raise AssertionError("two-buffer lookahead did not maintain gap-free playback")
        if result["finish_at"] < result["audio_end"]:
            raise AssertionError("final sample would be truncated")
    late = simulate_queue(counts, [0.0, 0.101] + [0.0] * 9)
    if not late["aborted"]:
        raise AssertionError("late refill must stop rather than continue after its timing budget")


def validate_default(source):
    if SAMPLE_IMPORT.search(source) or "warp-samples-enabled true" in source:
        raise AssertionError("generic app must not import or enable private samples")
    if "warp-samples-enabled" in source or "warp-song-number" in source:
        raise AssertionError("generic app must not define private sample-playback configuration")
    if "(= cmd warp-song-number)" in source or "(play-warp-samples)" in source:
        raise AssertionError("generic app must not reserve or route streamed song 7")
    if "@private-warp-dispatch" not in source:
        raise AssertionError("private generator dispatch point missing")


def private_runtime_source(source, repl):
    imports = sorted(SAMPLE_IMPORT.findall(source), key=int)
    if imports != [str(i) for i in range(12)]:
        raise AssertionError("private app must import all twelve sample chunks")
    selected = {}
    for form in forms(source):
        match = re.match(r"\((?:def|defun)\s+([^\s()]+)", form)
        if match and match[1] in SAMPLE_FUNCTIONS | SAMPLE_CONFIG:
            if match[1] in selected:
                raise AssertionError("duplicate private sample definition: " + match[1])
            if match[1] == "play-current-song":
                form = form.replace("(ext-midi-open 0 song-buf-0)",
                                    "(record-midi-open 0 song-buf-0)")
                form = form.replace("(ext-midi-open 1 song-buf-1)",
                                    "(record-midi-open 1 song-buf-1)")
            if match[1] == "warp-sample-rate":
                form = "(def warp-sample-rate 1000.0)"
            selected[match[1]] = form
    if selected.keys() != SAMPLE_FUNCTIONS | SAMPLE_CONFIG:
        missing = (SAMPLE_FUNCTIONS | SAMPLE_CONFIG) - selected.keys()
        raise AssertionError("missing private sample definitions: " + repr(missing))

    prelude = r'''
(def music-play-generation 0)
(def music-start-count 0)
(def music-end-reason 0)
(def current-song 0)
(def music-playing false)
(def music-starting false)
(def music-active-tracks 0)
(def music-master-vol 0.5)
(def music-t-start (systime))
(def channels (map (fn (x) (list x 0 0 0 0)) (range 4)))
(def warp-sample-0 (array-create 41))
(def warp-sample-1 (array-create 41))
(def warp-sample-2 (array-create 41))
(def warp-sample-3 (array-create 41))
(def warp-sample-4 (array-create 41))
(def warp-sample-5 (array-create 41))
(def warp-sample-6 (array-create 41))
(def warp-sample-7 (array-create 41))
(def warp-sample-8 (array-create 41))
(def warp-sample-9 (array-create 41))
(def warp-sample-10 (array-create 41))
(def warp-sample-11 (array-create 21))
(def warp-sample-chunks (list warp-sample-0 warp-sample-1 warp-sample-2
                              warp-sample-3 warp-sample-4 warp-sample-5
                              warp-sample-6 warp-sample-7 warp-sample-8
                              warp-sample-9 warp-sample-10 warp-sample-11))
(def warp-play-buffer-a (array-create 4000))
(def warp-play-buffer-b (array-create 4000))
(def warp-play-buffer-tail (array-create 160))
(def calls 0)
(def gen1-calls 0)
(def gen2-calls 0)
(def gen-a 0)
(def gen-b 0)
(def stops 0)
(def statuses 0)
(def opens 0)
(def samples-valid true)
(defun foc-play-samples (samples rate voltage) {
  (setq samples-valid (and samples-valid (= rate 1000.0) (= voltage 0.075)
       (or (= (buflen samples) 4000) (= (buflen samples) 160))))
  (if (= music-play-generation 1) (setq gen1-calls (+ gen1-calls 1)))
  (if (= music-play-generation 2) (setq gen2-calls (+ gen2-calls 1)))
  (setq calls (+ calls 1))
  true
})
(defun foc-play-stop () { (setq stops (+ stops 1)) true })
(defun send-music-status () { (setq statuses (+ statuses 1)) true })
(defun record-midi-open (slot data) { (setq opens (+ opens 1)) true })
(defun capture-generation (slot) {
  (var value (advance-music-generation))
  (atomic (if (= slot 0) (setq gen-a value) (setq gen-b value)))
})
(defun engine-pause () true)
(defun play-midi-track (parser volume decay song generation trim) true)
(defun reset-test-state () {
  (setq music-play-generation 0)
  (setq current-song 0)
  (setq music-playing false)
  (setq music-starting false)
  (setq music-active-tracks 0)
  (setq calls 0)
  (setq gen1-calls 0)
  (setq gen2-calls 0)
  (setq gen-a 0)
  (setq gen-b 0)
  (setq stops 0)
  (setq statuses 0)
  (setq opens 0)
  (setq samples-valid true)
})
(def song-buf-0-ready true)
(def song-buf-1-ready false)
(def song-buf-0 (array-create 2048))
(def song-buf-1 (array-create 2048))
(def song-buf-0-vol 1.0)
(def song-buf-0-decay 0.92)
(def resident-song-number 3)
'''
    checks = r'''
; Concurrent callers must receive distinct generation identities.
(reset-test-state)
(spawn 100 capture-generation 0)
(spawn 100 capture-generation 1)
(sleep 0.01)
(if (and (> gen-a 0) (> gen-b 0) (!= gen-a gen-b))
    (print "WARP_UNIQUE_GENERATION_PASS")
    (print (list "FAIL GENERATION" gen-a gen-b music-play-generation)))

; Same-song replay: old worker must not refill or finish the new playback.
(reset-test-state)
(play-warp-samples)
(sleep 0.001)
(play-warp-samples)
(sleep 0.65)
(if (and (= gen1-calls 2) (= gen2-calls 12) (= statuses 1)
         (= current-song 0) (not music-playing) samples-valid)
    (print "WARP_SAME_SONG_REPLAY_PASS")
    (print (list "FAIL SAME" calls gen1-calls gen2-calls statuses current-song music-playing samples-valid)))

; Stop wins while a new play is still in its startup wait, before chunk zero.
(reset-test-state)
(spawn 100 stop-music)
(play-warp-samples)
(sleep 0.15)
(if (and (= calls 0) (= current-song 0) (not music-playing))
    (print "WARP_STOP_BEFORE_FIRST_PASS")
    (print (list "FAIL PRESTART" calls current-song music-playing)))

; Switching to a streamed MIDI song invalidates a sleeping sample worker.
(reset-test-state)
(play-warp-samples)
(sleep 0.001)
(play-current-song)
(sleep 0.20)
(if (and (= gen1-calls 2) (= calls 2) (= opens 1)
         (= current-song 3) music-playing samples-valid)
    (print "WARP_SWITCH_TO_MIDI_PASS")
    (print (list "FAIL MIDI" gen1-calls calls current-song music-playing samples-valid)))

; A stop during the refill wait invalidates the worker and owns the stop.
(reset-test-state)
(play-warp-samples)
(sleep 0.001)
(stop-music)
(sleep 0.20)
(if (and (= gen1-calls 2) (= calls 2) (= current-song 0)
         (not music-playing) (= statuses 0) samples-valid)
    (print "WARP_STOP_DURING_WAIT_PASS")
    (print (list "FAIL STOP" gen1-calls calls current-song music-playing statuses samples-valid)))
'''
    program = prelude + "\n" + "\n".join(
        selected[name] for name in sorted(selected)) + checks
    with tempfile.NamedTemporaryFile("w", suffix=".lbm") as program_file:
        program_file.write(program)
        program_file.flush()
        result = subprocess.run(
            [repl, "--silent", "--history_file=", "--heap_size=8192",
             "--memory_size=28672", "--src", program_file.name, "--terminate"],
            capture_output=True, text=True, timeout=20)
    output = result.stdout + result.stderr
    markers = (
        "WARP_UNIQUE_GENERATION_PASS", "WARP_SAME_SONG_REPLAY_PASS",
        "WARP_STOP_BEFORE_FIRST_PASS",
        "WARP_SWITCH_TO_MIDI_PASS", "WARP_STOP_DURING_WAIT_PASS",
    )
    if result.returncode or any(marker not in output for marker in markers):
        raise SystemExit("private LispBM sample runtime failed (exit %d):\n%s" %
                         (result.returncode, output))
    print("private LispBM asynchronous tests passed: unique generations, replay, pre-start stop, MIDI switch, wait stop")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repl", default=os.environ.get("LBM_REPL", "lbm"))
    parser.add_argument("--source", type=Path, default=ROOT / "app.lisp")
    args = parser.parse_args()
    source = args.source.read_text()
    if args.source.resolve() == (ROOT / "app.lisp").resolve():
        validate_default(source)
        print("default app configuration passed: private samples disabled; song 7 is not intercepted")
        return
    check_queue_model()
    private_runtime_source(source, args.repl)
    print("two-buffer scheduler model passed: bounded wake lateness, abort limit, complete final sample")


if __name__ == "__main__":
    main()
