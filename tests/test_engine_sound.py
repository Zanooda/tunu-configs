import importlib.util
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('engine_forms', ROOT / 'tests/test_drive_limits.py')
reader = importlib.util.module_from_spec(spec)
spec.loader.exec_module(reader)

MUSIC_NAMES = {'music-play-generation', 'music-active-tracks', 'music-starting', 'music-upload-last',
               'advance-music-generation', 'music-generation-current', 'music-track-finished',
               'stop-music', 'play-current-song', 'play-axelf', 'play-midi-track', 'note-on', 'note-off', 'vscale'}

PRELUDE = r'''
(def failures 0)
(def clock 0.0)
(def native-speed 0.0)
(def throttle 0.0)
(def brake 0.0)
(def fault 0)
(def inject-error false)
(def speed-cal 1.03)
(def music-playing false)
(def current-song 0)
(def music-t-start (systime))
(def music-master-vol 0.5)
(def music-start-count 0)
(def music-end-reason 0)
(def channels (map (fn (x) (list x 0 0 0 0)) (range 4)))
(def dataArray_0x7ED (array-create 4))
(def music-starting false)
(def music-upload-last -100.0)
(def max-voltage-sum 0.0)
(def max-frequency 0.0)
(def min-frequency 100000.0)
(def status-events 0)
(def song-status-events 0)
(def tone-events 0)
(def stop-events 0)
(def frequencies (list 500.0 500.0 500.0 500.0))
(def voltages (list 0.0 0.0 0.0 0.0))
(defun expect (value label) {
    (if (not value) { (def failures (+ failures 1)) (print (list "FAIL" label)) })
})
(defun clock-now () clock)
(defun clock-age (start) (- clock start))
(defun get-speed () (if inject-error nil native-speed))
(defun get-adc-decoded (channel) (if (= channel 0) throttle brake))
(defun get-fault () fault)
(defun foc-play-tone (channel frequency voltage) {
    (setix frequencies channel frequency)
    (setix voltages channel voltage)
    (def tone-events (+ tone-events 1))
    (if (> (voltage-sum) max-voltage-sum) (def max-voltage-sum (voltage-sum)))
    (if (> voltage 0.0) {
        (if (> frequency max-frequency) (def max-frequency frequency))
        (if (< frequency min-frequency) (def min-frequency frequency))
    })
    true
})
(defun fake-sleep (secs) (def clock (+ clock secs)))
(defun foc-play-stop () {
    (looprange channel 0 4 { (setix voltages channel 0.0) })
    (def stop-events (+ stop-events 1))
    true
})
(defun can-send-sid (id data) {
    (expect (and (= id 0x7ED) (= (buflen data) 4)) "engine status frame")
    (def status-events (+ status-events 1))
    true
})
(defun send-music-status () { (def song-status-events (+ song-status-events 1)) true })
(defun command (cmd volume style) {
    (var data (array-create 3))
    (bufset-u8 data 0 cmd)
    (bufset-u8 data 1 volume)
    (bufset-u8 data 2 style)
    (handle-engine-command data)
})
(defun voltage-sum () (+ (ix voltages 0) (ix voltages 1) (ix voltages 2) (ix voltages 3)))
'''


class EngineSoundTests(unittest.TestCase):
    def selected_source(self, fake_clock=True):
        selected = []
        source = Path(os.environ.get('ENGINE_APP_SOURCE', str(ROOT / 'app.lisp')))
        for form in reader.forms(source.read_text()):
            match = re.match(r'\((?:def|defun)\s+([^\s()]+)', form)
            if match and (match[1].startswith('engine-') or match[1] in MUSIC_NAMES
                          or match[1] in ('handle-engine-command', 'send-engine-status', 'send-engine-status-locked')):
                form = form.replace('(ext-midi-open ', '(mock-midi-open ')
                form = form.replace('(ext-midi-parse parser-idx)', '(mock-midi-parse parser-idx generation)')
                if fake_clock:
                    form = form.replace('(systime)', '(clock-now)').replace('(secs-since ', '(clock-age ')
                    form = form.replace('(sleep ', '(fake-sleep ')
                selected.append(form)
        return '\n'.join(selected)

    def run_lisp(self, checks, fake_clock=True):
        repl = os.environ.get('LBM_REPL', 'lbm')
        if not shutil.which(repl):
            self.skipTest('set LBM_REPL to the host LispBM executable')
        program = PRELUDE + '\n' + self.selected_source(fake_clock) + '\n' + checks + """
(if (= failures 0) (print "ENGINE_TESTS_PASS") (print (list "ENGINE_TESTS_FAIL" failures)))
"""
        with tempfile.NamedTemporaryFile('w', suffix='.lbm') as script:
            script.write(program)
            script.flush()
            result = subprocess.run([repl, '--silent', '--history_file=', '--heap_size=16384',
                                     '--memory_size=32768', '--src', script.name, '--terminate'],
                                    capture_output=True, text=True, timeout=60)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('ENGINE_TESTS_PASS', result.stdout, result.stdout + result.stderr)
        self.assertNotIn('("FAIL"', result.stdout, result.stdout + result.stderr)

    def test_commands_start_stop_and_status(self):
        self.run_lisp(r"""
(engine-init)
(expect (and (not engine-enabled) (not engine-sounding) (= engine-vol-pct 50) (= engine-style 0)) "silent boot")
(command 1 60 0)
(expect (and engine-enabled (= engine-vol-pct 60) (= (bufget-u8 dataArray_0x7ED 0) 1)
             (= (bufget-u8 dataArray_0x7ED 1) 60)) "on is echoed")
(engine-step)
(expect (and engine-running engine-sounding (> (voltage-sum) 0.0)) "start sound, then running")
(command 1 60 0)
(engine-step)
(expect (and engine-running engine-sounding) "repeated on does not restart")
(command 0 60 0)
(engine-step)
(expect (and (not engine-running) (not engine-sounding) (= (voltage-sum) 0.0)) "off plays the stop sound and goes silent")
(def before status-events)
(def clock (+ clock 1.5))
(engine-step)
(expect (> status-events before) "status repeated once per second")
""")

    def test_malformed_commands_and_volume_clamp(self):
        self.run_lisp(r"""
(engine-init)
(command 3 50 0)
(expect (not engine-enabled) "unknown command ignored")
(command 1 50 5)
(expect (and (not engine-enabled) (= engine-style 0)) "unknown style ignores the command")
(handle-engine-command (array-create 0))
(expect (not engine-enabled) "empty frame ignored")
(command 0 255 0)
(expect (and (= engine-vol-pct 100) (= engine-vol (engine-vol-max))) "volume clamps to 100 %")
(command 0 70 2)
(expect (and (= engine-style 2) (= (bufget-u8 dataArray_0x7ED 3) 2)) "style is applied and echoed")
""")

    def test_music_upload_and_fault_keep_it_quiet(self):
        self.run_lisp(r"""
(engine-init)
(command 1 100 0)
(engine-step)
(expect engine-sounding "sounding before")
(engine-pause)
(expect (and (not engine-sounding) (= (voltage-sum) 0.0)) "pause releases the channels")
(def music-playing true)
(def before tone-events)
(engine-step)
(expect (and (not engine-sounding) (= tone-events before)) "silent while a song plays")
(def music-playing false)
(def music-starting true)
(engine-out 100.0 1.0 0.02)
(engine-raw 100.0 1.0 100.0 1.0 100.0 1.0 100.0 1.0)
(expect (= tone-events before) "no writes while a song starts")
(def music-starting false)
(def music-upload-last clock)
(engine-step)
(expect (= tone-events before) "silent right after upload traffic")
(def clock (+ clock 1.0))
(engine-step)
(expect (and engine-sounding (> (voltage-sum) 0.0)) "resumes afterwards")
(def fault 1)
(engine-step)
(expect (and (not engine-sounding) (= (voltage-sum) 0.0)) "fault silences")
(def fault 0)
(command 0 100 0)
(def music-playing true)
(engine-step)
(expect (and (not engine-running) (= (voltage-sum) 0.0)) "off during a song needs no stop sound")
""")

    def test_demo_every_style_is_bounded_and_completes(self):
        self.run_lisp(r"""
(engine-init)
(looprange style 0 5 {
    (def max-voltage-sum 0.0)
    (command 2 100 style)
    (engine-step)
    (expect (and (not engine-demo) (not engine-sounding) (= (voltage-sum) 0.0)) "demo completes and goes silent")
    (expect (and (> max-voltage-sum 0.0) (<= max-voltage-sum (* 2.2501 (engine-vol-max)))) "voltage within the cap")
})
(expect (and (>= min-frequency 5.0) (<= max-frequency 5000.0)) "frequencies stay in range")
(expect (not engine-enabled) "demo leaves the on/off request unchanged")
(def music-playing true)
(command 2 100 0)
(engine-step)
(expect (and (not engine-demo) (= (voltage-sum) 0.0)) "a song stops the demo")
""")

    def test_virtual_gears_and_braked_revving(self):
        self.run_lisp(r"""
(engine-init)
(expect (> (engine-target-rpm 9.1) (engine-target-rpm 9.3)) "upshift drops the pitch")
(expect (< (engine-target-rpm 2.0) (engine-target-rpm 8.0)) "pitch climbs within a gear")
(engine-reset)
(looprange i 0 50 { (engine-tick 0.0 1.0 0.6) })
(expect (> engine-rpm (* 1.5 (engine-idle))) "revs at a standstill with the brake held")
""")

    def test_upload_during_startup_releases_tones(self):
        self.run_lisp(r"""
(def interrupted nil)
(defun fake-sleep (secs) {
    (def clock (+ clock secs))
    (if (and (not interrupted) (> (voltage-sum) 0.0)) {
        (def interrupted t)
        (def music-upload-last clock)
        (engine-pause)
    })
})
(engine-init)
(command 1 100 0)
(engine-step)
(expect (and interrupted (engine-quiet) (not engine-sounding) (= (voltage-sum) 0.0)) "upload releases start tones")
""")

    def test_fault_and_worker_error_during_startup_release_tones(self):
        self.run_lisp(r"""
(def interrupted nil)
(defun fake-sleep (secs) {
    (def clock (+ clock secs))
    (if (and (not interrupted) (> (voltage-sum) 0.0)) {
        (def interrupted t)
        (def fault 1)
    })
})
(engine-init)
(command 1 100 0)
(engine-step)
(expect (and interrupted (not engine-sounding) (= (voltage-sum) 0.0)) "fault releases start tones")
(def fault 0)
(def interrupted nil)
(defun fake-sleep (secs) {
    (def clock (+ clock secs))
    (if (and (not interrupted) (> (voltage-sum) 0.0)) {
        (def interrupted t)
        (/ 1 0)
    })
})
(engine-init)
(command 1 100 0)
(engine-loop)
(expect (and interrupted (not engine-enabled) (not engine-sounding) (= (voltage-sum) 0.0)) "error releases start tones")
""")

    def test_pause_does_not_clear_foreground_notes(self):
        self.run_lisp(r"""
(engine-init)
(engine-raw 100.0 1.0 150.0 1.0 200.0 1.0 250.0 1.0)
(setix (ix channels 0) 1 60)
(foc-play-tone 0 261.6 0.7)
(def music-playing true)
(engine-pause)
(expect (and (not engine-sounding) (= (ix voltages 0) 0.7) (= (ix voltages 1) 0.0)) "pause preserves a foreground note")
""")

    def test_sequences_keep_reporting_status(self):
        self.run_lisp(r"""
(engine-init)
(command 1 50 0)
(def before status-events)
(engine-step)
(expect (>= (- status-events before) 3) "status continues during startup")
(command 2 50 0)
(def before status-events)
(engine-step)
(expect (>= (- status-events before) 20) "status continues during demo")
""")

    def test_demo_takes_over_startup_on_the_same_worker(self):
        self.run_lisp(r"""
(def renderer 0)
(def other-writes 0)
(def demo-runs 0)
(def original-tone foc-play-tone)
(def original-demo engine-demo-ride)
(defun foc-play-tone (channel frequency voltage) {
    (if (and (> voltage 0.0) (!= (self) renderer))
        (def other-writes (+ other-writes 1)))
    (original-tone channel frequency voltage)
})
(defun engine-demo-ride () {
    (def demo-runs (+ demo-runs 1))
    (original-demo)
})
(engine-init)
(command 1 50 0)
(def renderer (spawn 300 engine-loop))
(sleep 0.1)
(expect (> tone-events 0) "startup is active")
(command 2 50 0)
(sleep 0.2)
(expect (and engine-demo (= demo-runs 1) (= other-writes 0)) "one worker owns startup and demo")
(command 2 50 0)
(sleep 0.1)
(expect (= demo-runs 1) "demo retry does not restart")
(command 0 50 0)
(sleep 0.15)
(expect (not engine-demo) "off cancels the demo")
""", fake_clock=False)

    def test_all_sequences_fit_controller_worker_stack(self):
        self.run_lisp(r"""
(engine-init)
(def worker-done false)
(def worker-result nil)
(spawn 300 (fn () {
    (def worker-result (trap {
        (looprange style 0 5 {
            (command 1 100 style)
            (engine-step)
            (command 2 100 style)
            (engine-step)
            (command 0 100 style)
            (engine-step)
        })
        t
    }))
    (def worker-done true)
}))
(looprange i 0 500 {
    (if worker-done (break))
    (sleep 0.01)
})
(expect (and worker-done (eq worker-result '(exit-ok t))) "all sequences fit 300-cell worker")
(expect (= (voltage-sum) 0.0) "worker ends silent")
""")

    def test_worker_error_releases_channels(self):
        self.run_lisp(r"""
(engine-init)
(command 1 100 0)
(engine-step)
(def inject-error true)
(engine-loop)
(expect (and (not engine-enabled) (not engine-running) (= (voltage-sum) 0.0)
             (= (bufget-u8 dataArray_0x7ED 0) 0)) "error stops the loop, silent and reported off")
""")


if __name__ == '__main__':
    unittest.main()
