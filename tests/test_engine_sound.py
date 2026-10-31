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
    true
})
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
    def selected_source(self, fake_clock=False):
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
                selected.append(form)
        return '\n'.join(selected)

    def run_lisp(self, checks, fake_clock=False):
        repl = os.environ.get('LBM_REPL', 'lbm')
        if not shutil.which(repl):
            self.skipTest('set LBM_REPL to the host LispBM executable')
        program = PRELUDE + '\n' + self.selected_source(fake_clock) + '\n' + checks + '''
(if (= failures 0) (print "ENGINE_TESTS_PASS") (print (list "ENGINE_TESTS_FAIL" failures)))
'''
        with tempfile.NamedTemporaryFile('w', suffix='.lbm') as script:
            script.write(program)
            script.flush()
            result = subprocess.run([repl, '--silent', '--history_file=', '--heap_size=16384',
                                     '--memory_size=32768', '--src', script.name, '--terminate'],
                                    capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('ENGINE_TESTS_PASS', result.stdout, result.stdout + result.stderr)
        self.assertNotIn('("FAIL"', result.stdout, result.stdout + result.stderr)

    def test_commands_styles_and_bounded_demo_model(self):
        self.run_lisp(r'''
(engine-init)
(expect (and (not engine-enabled) (not engine-owned) (= engine-volume 50)) "silent boot")
(looprange volume 0 101 {
    (command 1 volume 0)
    (expect (= (bufget-u8 dataArray_0x7ED 1) volume) "exact percent echo")
})
(def clock 1.0)
(engine-step)
(def first-start engine-cue-start)
(def clock 1.1)
(command 1 50 0)
(expect (= engine-cue-start first-start) "on retry does not restart cue")
(command 0 50 0)
(def first-stop engine-cue-start)
(def clock 1.2)
(command 0 50 0)
(expect (= engine-cue-start first-stop) "off retry does not extend cue")
(def clock 1.6)
(engine-step)
(expect (and (not engine-owned) (= (voltage-sum) 0.0)) "off finishes")
(command 2 50 0)
(def first-demo engine-demo-start)
(def clock 1.8)
(command 2 50 0)
(expect (= engine-demo-start first-demo) "demo retry is idempotent")
(engine-step)
(expect (and engine-demo-playing (not engine-enabled) engine-owned) "standalone demo")
(command 0 50 0)
(expect (and (not engine-demo-playing) (not engine-owned) (= (voltage-sum) 0.0)) "demo stop is immediate")
(def bad (array-create 1))
(handle-engine-command bad)
(command 9 50 0)
(command 1 50 99)
(expect (and (not engine-enabled) (= engine-style 0)) "malformed commands ignored")
(command 1 255 0)
(expect (= engine-volume 100) "volume clamps")
(engine-init)
(command 1 50 0)
(def clock (+ clock 0.02))
(engine-step)
(def fault 1)
(def clock (+ clock 0.02))
(engine-step)
(expect (and (not engine-owned) (= (voltage-sum) 0.0)) "fault silences")
(def fault 0)
(looprange style 0 5 {
    (engine-init)
    (command 2 100 style)
    (expect (= (bufget-u8 dataArray_0x7ED 3) style) "style echo")
    (looprange step 0 1520 {
        (def clock (+ clock 0.02))
        (engine-step)
        (expect (and (>= engine-gain 0.0) (<= engine-gain 1.0)
                     (>= engine-rpm 0.0) (<= engine-rpm 11500.0)) "bounded model")
        (expect (<= (voltage-sum) 1.7) "bounded total modulation")
        (loopforeach frequency frequencies {
            (expect (and (>= frequency 70.0) (<= frequency 5000.0)) "bounded frequency")
        })
        (expect (= native-speed 0.0) "demo never changes native speed")
    })
    (expect (and (not engine-demo-playing) (not engine-owned) (= (voltage-sum) 0.0)) "demo completes")
})
(def profile (engine-profile 0))
(expect (> (engine-target-rpm profile 0.0 1.0 1.0)
           (engine-target-rpm profile 0.0 1.0 0.0)) "braked free rev")
(expect (> (engine-target-rpm profile 9.9 0.8 0.0)
           (engine-target-rpm profile 10.1 0.8 0.0)) "virtual gear pitch drop")
''', fake_clock=True)

    def test_music_priority_upload_and_worker_error_cleanup(self):
        self.run_lisp(r'''
(engine-init)
(command 1 50 0)
(def clock 0.02)
(engine-step)
(expect engine-owned "engine owns idle audio")
(engine-pause)
(def music-playing true)
(def tone-events 0)
(def clock 0.04)
(engine-step)
(command 0 50 0)
(expect (= tone-events 0) "engine off cannot mute a foreground song")
(command 1 50 0)
(engine-step)
(expect (= tone-events 0) "music has priority over engine start")
(def music-playing false)
(def music-starting true)
(engine-step)
(expect (= tone-events 0) "startup reservation has priority")
(def music-starting false)
(def music-upload-last clock)
(engine-step)
(expect (= tone-events 0) "upload has priority")
(def clock 0.5)
(engine-step)
(expect (and engine-owned (> tone-events 0)) "engine resumes after music")
(def inject-error true)
(engine-loop)
(expect (and (not engine-owned) (not engine-enabled) (= (voltage-sum) 0.0)) "worker failure cleans up")
''', fake_clock=True)

    def test_native_tick_clock_gain_and_live_input_response(self):
        self.run_lisp(r'''
(defun clock-now () (to-u (* clock 10000.0)))
(defun clock-age (start) (/ (to-float (- (clock-now) start)) 10000.0))
(def clock 1.0)
(engine-init)
(command 1 50 0)
(def clock 1.02)
(engine-step)
(expect (and (> engine-rpm 179.0) (< engine-rpm 181.0)) "20ms tick clock gives 180rpm slew")
(expect (and (> engine-gain 0.099) (< engine-gain 0.101)) "20ms gain slew")
(looprange step 0 60 {
    (def clock (+ clock 0.02))
    (engine-step)
})
(def idle-frequency (ix frequencies 0))
(expect (> (ix voltages 0) 0.15) "50 percent idle is above the native tone cutoff")
(expect (> (voltage-sum) 0.3) "audible idle modulation")
(def native-speed (/ (* 20.0 speed-cal) 3.6))
(looprange step 0 60 { (def clock (+ clock 0.02)) (engine-step) })
(expect (and (> engine-model-speed 19.9) (< engine-model-speed 20.1)) "uses calibrated live speed")
(expect (> (ix frequencies 0) (* idle-frequency 3.0)) "road speed clearly raises pitch")
(def rolling-frequency (ix frequencies 0))
(def throttle 1.0)
(looprange step 0 60 { (def clock (+ clock 0.02)) (engine-step) })
(expect (> (ix frequencies 0) (+ rolling-frequency 25.0)) "live throttle raises pitch")
(expect (> engine-gain 0.49) "live throttle raises level")
(def native-speed 0.0)
(def brake 1.0)
(looprange step 0 60 { (def clock (+ clock 0.02)) (engine-step) })
(expect (> engine-rpm 5000.0) "braked stationary throttle revs")
(expect (and (>= engine-wobble-phase 0.0) (< engine-wobble-phase 6.283186)) "phase stays bounded independently of tick magnitude")
''', fake_clock=True)

    def test_spawned_worker_uses_controller_stack_budget(self):
        self.run_lisp(r'''
(engine-init)
(command 1 50 0)
(spawn 300 engine-loop)
(sleep 0.25)
(expect (and engine-enabled engine-owned (> tone-events 0)
             (< (secs-since engine-last-step) 0.1)) "spawned worker stays alive")
(def inject-error true)
(sleep 0.1)
(expect (and (not engine-enabled) (not engine-owned) (= (voltage-sum) 0.0)) "spawned worker cleans up")
''')

    def test_natural_midi_completion_and_same_song_replay(self):
        self.run_lisp(r'''
(def song-buf-0-ready true)
(def song-buf-1-ready true)
(def song-buf-0 (array-create 8))
(def song-buf-1 (array-create 8))
(def song-buf-0-vol 0.1)
(def song-buf-1-vol 0.1)
(def song-buf-0-decay 1.0)
(def song-buf-1-decay 1.0)
(def resident-song-number 9)
(def axelf-song-number 1)
(def parse-offsets (list 0 0))
(def parser-generations (list 0 0))
(def stale-parses 0)
(defun mock-midi-open (slot data) {
    (setix parse-offsets slot 0)
    (setix parser-generations slot music-play-generation)
    true
})
(defun mock-midi-parse (slot generation) {
    (if (!= generation (ix parser-generations slot)) (def stale-parses (+ stale-parses 1)))
    (var offset (ix parse-offsets slot))
    (setix parse-offsets slot (+ offset 1))
    (cond
        ((= offset 0) '(midi-header 6 0 1 100))
        ((= offset 1) (list 'midi-track-midi 0 9 0 (+ 48 (* slot 12)) 100))
        ((= offset 2) (list 'midi-track-midi (+ 10 (* slot 10)) 8 0 (+ 48 (* slot 12)) 0))
        (t '(midi-track-meta 0 47 0 0)))
})
(engine-init)
(command 1 50 0)
(sleep 0.03)
(engine-step)
(play-current-song)
(expect (and music-playing (not engine-owned)) "MIDI takes audio ownership")
(def tone-before tone-events)
(engine-step)
(expect (= tone-events tone-before) "engine does not overwrite MIDI")
(sleep 0.12)
(expect (and (not music-playing) (= current-song 0) (= music-active-tracks 0)
             (= song-status-events 1)) "both tracks finish naturally")
(engine-step)
(expect engine-owned "engine resumes after natural MIDI end")
(def song-status-events 0)
(play-current-song)
(sleep 0.001)
(play-current-song)
(sleep 0.12)
(expect (and (= stale-parses 0) (= song-status-events 1) (not music-playing)) "same-song generations cannot interfere")
(def music-playing true)
(def current-song 9)
(def music-active-tracks 1)
(def old-generation music-play-generation)
(advance-music-generation)
(music-track-finished old-generation 0)
(expect (and music-playing (= current-song 9) (= music-active-tracks 1)) "stale completion cannot finish a new song")
''')

    def test_engine_code_never_changes_riding_configuration(self):
        source = self.selected_source()
        engine = '\n'.join(form for form in reader.forms(source)
                           if form.startswith('(defun engine-') or form.startswith('(defun handle-engine-command'))
        for forbidden in ('conf-set', 'set-current', 'set-duty', 'set-rpm', 'app-adc-override', 'eeprom-store'):
            self.assertNotIn('(' + forbidden, engine)


if __name__ == '__main__':
    unittest.main()
