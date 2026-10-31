#!/usr/bin/env python3
"""Exercise the actual MIDI playback functions with a host LispBM REPL."""
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]


def forms(source):
    stack = []
    start = None
    string = escape = comment = False
    for i, char in enumerate(source):
        if comment:
            comment = char != '\n'
        elif string:
            if escape:
                escape = False
            elif char == '\\':
                escape = True
            elif char == '"':
                string = False
        elif char == ';':
            comment = True
        elif char == '"':
            string = True
        elif char in '({[':
            if not stack:
                start = i
            stack.append(char)
        elif char in ')}]':
            assert stack and stack.pop() == {')': '(', '}': '{', ']': '['}[char]
            if not stack:
                yield source[start:i + 1]
    assert not stack and not string


PRELUDE = r'''
(def failures 0)
(def music-playing false)
(def current-song 0)
(def music-t-start (systime))
(def music-master-vol 0.5)
(def music-start-count 0)
(def music-end-reason 0)
(def channels (map (fn (x) (list x 0 0 0 0)) (range 4)))
(def song-status-events 0)
(def tone-events 0)
(def stop-events 0)
(def song-buf-0-ready true)
(def song-buf-1-ready true)
(def song-buf-0 (array-create 8))
(def song-buf-1 (array-create 8))
(def song1-track0 (array-create 8))
(def song1-track1 (array-create 8))
(def song-buf-0-vol 0.1)
(def song-buf-1-vol 0.1)
(def song-buf-0-decay 1.0)
(def song-buf-1-decay 1.0)
(def resident-song-number 9)
(def axelf-song-number 1)
(def parse-offsets (list 0 0))
(def parser-generations (list 0 0))
(def stale-parses 0)
(defun expect (value label) {
    (if (not value) { (def failures (+ failures 1)) (print (list "FAIL" label)) })
})
(defun foc-play-tone (channel frequency voltage) { (def tone-events (+ tone-events 1)) true })
(defun foc-play-stop () { (def stop-events (+ stop-events 1)) true })
(defun send-music-status () { (def song-status-events (+ song-status-events 1)) true })
(defun engine-pause () true)
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
'''


class MidiLifecycleTests(unittest.TestCase):
    def test_lifecycle(self):
        repl = os.environ.get('LBM_REPL', 'lbm')
        if not shutil.which(repl):
            self.skipTest('set LBM_REPL to the host LispBM executable')
        names = {'music-play-generation', 'music-active-tracks', 'music-starting',
                 'advance-music-generation', 'music-generation-current', 'music-track-finished',
                 'vscale', 'note-on', 'note-off', 'play-midi-track', 'stop-music',
                 'play-current-song', 'play-axelf'}
        selected = []
        for form in forms((ROOT / 'app.lisp').read_text()):
            match = re.match(r'\((?:def|defun)\s+([^\s()]+)', form)
            if match and match[1] in names:
                form = form.replace('(ext-midi-open ', '(mock-midi-open ')
                form = form.replace('(ext-midi-parse parser-idx)', '(mock-midi-parse parser-idx generation)')
                selected.append(form)
        self.assertEqual(len(selected), len(names))
        checks = r'''
(play-current-song)
(expect music-playing "song starts")
(sleep 0.12)
(expect (and (not music-playing) (= current-song 0) (= music-active-tracks 0)
             (= song-status-events 1)) "both tracks finish naturally")
(def song-status-events 0)
(play-current-song)
(sleep 0.001)
(play-current-song)
(sleep 0.12)
(expect (and (= stale-parses 0) (= song-status-events 1) (not music-playing)) "same-song replay isolates generations")
(def music-playing true)
(def current-song 9)
(def music-active-tracks 1)
(def old-generation music-play-generation)
(advance-music-generation)
(music-track-finished old-generation 0)
(expect (and music-playing (= current-song 9) (= music-active-tracks 1)) "stale completion ignored")
(def tone-events 0)
(note-on 50 1.0 1.0 old-generation)
(expect (= tone-events 0) "stale note ignored")
(stop-music)
(expect (and (not music-playing) (= current-song 0)) "stop clears playback")
(play-axelf)
(sleep 0.12)
(expect (and (not music-playing) (= current-song 0)) "native package finishes")
(spawn 200 play-current-song)
(sleep 0.01)
(expect music-starting "startup reserves playback")
(stop-music)
(sleep 0.08)
(expect (and (not music-starting) (not music-playing) (= current-song 0)) "stop cancels pending start")
(if (= failures 0) (print "MIDI_TESTS_PASS") (print (list "MIDI_TESTS_FAIL" failures)))
'''
        program = PRELUDE + '\n' + '\n'.join(selected) + '\n' + checks
        with tempfile.NamedTemporaryFile('w', suffix='.lbm') as script:
            script.write(program)
            script.flush()
            result = subprocess.run([repl, '--silent', '--history_file=', '--heap_size=16384',
                                     '--memory_size=32768', '--src', script.name, '--terminate'],
                                    capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('MIDI_TESTS_PASS', result.stdout, result.stdout + result.stderr)
        self.assertNotIn('(\"FAIL\"', result.stdout, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()
