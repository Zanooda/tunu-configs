; ===== MIDI imports — must appear before any executable code =====
(import "pkg::midi@://vesc_packages/lib_midi/midi.vescpkg" 'midi)

; Axel F is imported from vesc_tool's built-in stock lib_files package.
(import "pkg::midi_axelf_melody@://vesc_packages/lib_files/files.vescpkg" 'song1-track0)
(import "pkg::midi_axelf_base@://vesc_packages/lib_files/files.vescpkg"   'song1-track1)
; Private sample imports are added only to the generated sample-playback build.
; @private-warp-imports
; @boost-native-imports

(def can-cnt 0)
(def statusByte_1 0x00)
(def tick 0)

; 0x7E8 bytes 4-7: rated power (BCD kW), max speed (BCD km/h), base SW version
; (BCD nibbles), app revision (plain byte).
(def motor-power-kw 0x03)
(def max-speed-kmh 0x45)
(def sw-base-version 0x10)  ; "1.0"
(def sw-app-version 5)

(def pole-pairs 24.0)

; Empirical speed/odometer calibration, matches CalibrationFactor in ecu-service.
(def speed-cal 1.03)

; KERS state
(def kers-enabled false)
(def kers-current 0)
; Regen voltage target in mV (0x4E2, 10 mV units on the wire). Full battery
; regen authority at or below it, tapering linearly to zero across the band
; above it, so braking into a nearly full pack eases off instead of pushing it
; over the target.
(def kers-voltage 56000)
(def kers-voltage-taper 3000.0)
; Battery-side regen limit currently applied (A, negative), so the loop only
; writes the config when it actually changes.
(def kers-amps-now 0.0)

; Low-speed KERS fade, in ERPM so it tracks the FOC sensor handover directly.
; Coming down, the observer hands back to the halls between foc_sl_erpm (2000)
; and foc_sl_erpm_start (1800). Braking through that blend at full current is
; what rattles, so the fade bottoms out at a floor just above the blend and the
; floor carries through it, keeping the lever authoritative at crawl. The fade
; band sits at very low speed: full regen down to ~8.5 km/h, floor below that.
; Below the standstill cutoff the halls only give a coarse sector angle, and
; brake current there can turn into a slight forward pull, so the scale drops
; to zero — KERS on or off. A forward drive request bypasses both the cutoff
; and fade: VESC also applies this brake-side limit to forward torque while
; moving backwards.
; With 48 poles and the 0.416 m wheel: 2200 erpm ~ 7.2 km/h, 2600 ~ 8.5, 300 ~ 1.
(def kers-fade-start-erpm 2600.0)
(def kers-fade-end-erpm 2200.0)
(def kers-floor 0.25)
(def kers-hold-cutoff-erpm 300.0)
; Net throttle-minus-brake level, in ADC units, above which the rider counts as
; asking for drive. The ADC app zeroes anything below its own 0.15 deadband, so
; this is already a real command.
(def kers-drive-request-level 0.15)
; Battery charging allowance for a forward drive command, independent of KERS.
; Opposing backward motion can regenerate. The voltage taper and firmware
; battery/BMS limits still apply.
(def kers-drive-regen-floor -3.0)
; Max change in scale per 5 ms tick: engage ramps in fast but not as a step
; (~60 ms full sweep), and a noisy get-rpm in the fade band cannot chatter the
; limit. Forward drive uses the ADC app's torque ramp, not this brake fade.
(def kers-scale-slew 0.08)
; Is the rider asking for drive? The ADC app applies its own 0.15 deadband to
; the same net value before it issues any current, so this is the earliest
; honest signal that a drive command is on its way.
(defun drive-requested ()
    (> (- (get-adc-decoded 0) (get-adc-decoded 1)) kers-drive-request-level)
)

(def kers-scale 0.0)

; Shutdown detection: ADC channel 2 sits at ~2.63V while powered and drops on
; power loss. (Channel 3 reads ~2.23V on this unit, already below the
; threshold, so it fired right at boot instead.)
(def shutdown-threshold 2.48)
(def shutdown-triggered false)

; The optional native guard owns boost expiry independently of the Lisp evaluator.
(def boost-native-enabled false)
(def boost-policy-enabled false)
(def boost-per-pack-amps 85.0)
(def boost-duration-seconds 10.0)
(def boost-cooldown-seconds 90.0)
(def boost-heartbeat-ms 250)
(def boost-supply-ms 3000)
(def boost-release-ms 100)
(def boost-runtime-ready false)
(def boost-control-byte 1)

(defun boost-start () {
    (def boost-runtime-ready false)
    (def boost-control-byte 1)
    (if (and boost-native-enabled boost-policy-enabled)
        (match (trap (progn
            (load-native-lib boost-guard-lib)
            (and (= (ext-boost-api) 1)
                 (ext-boost-config boost-per-pack-amps boost-duration-seconds
                                   boost-cooldown-seconds boost-heartbeat-ms boost-supply-ms boost-release-ms))))
            ((exit-ok (? ready)) (def boost-runtime-ready ready))
            (_ (print "Boost unavailable; normal limits retained"))))
})

(defun boost-tick () {
    (if boost-runtime-ready
        (if (not (ext-boost-heartbeat boost-control-byte (if (drive-requested) 1 0)))
            (def boost-runtime-ready false)))
})

(defun boost-normal-current ()
    (if boost-runtime-ready (ext-boost-normal-current) (conf-get 'l-in-current-max)))

(defun boost-set-normal-current (amps packs)
    (and boost-runtime-ready (ext-boost-normal amps packs)))

(defun boost-supply-lease (packs normal-amps)
    (and boost-runtime-ready (ext-boost-supply packs normal-amps)))

; ===== Music: streamed song buffers =====
; music-service streams a song's decoded MIDI bytes over CAN
; (Music_Upload_Begin/Chunk/End, below) immediately before playing it, into
; two FIXED buffers that are only ever refilled, never recreated.
(def song-buf-max 2048)   ; headroom over the largest known track
(def song-buf-0-len 0)
(def song-buf-1-len 0)
(def song-buf-0-vol 1.0)
(def song-buf-1-vol 1.0)
(def song-buf-0-decay 0.92)
(def song-buf-1-decay 0.92)
(def song-buf-0-ready false)
(def song-buf-1-ready false)
(def resident-song-number 0)   ; number of the song currently loaded, 0 = none

; Upload-in-progress state. One track uploads at a time; Begin resets this.
(def upload-active-slot -1)    ; 0 or 1 mid-upload, -1 when idle
(def upload-checksum 0)        ; running sum-of-bytes mod 256
(def upload-received 0)        ; bytes received so far for the active upload

; Axel F's reserved song number — must match songs.json's "number" for the
; "Axel F" / "source": "builtin-package" entry on the music-service side.
(def axelf-song-number 1)
; @private-warp-config

; ===== Music control CAN IDs =====
; These extension messages share the 0x4E0-0x4EF range passed to LispBM.
; Music uses the following IDs in that block:
;
;   Music_Control_ID1     0x4E8  play/stop     byte0: 0x00 stop, N = play song N
;   Music_Control_ID2     0x4E9  master volume byte0: 0..100 (percent, live)
;   Engine_Sound          0x4EA  off/on/demo, volume percent, style 0..4
;   Music_Upload_Begin    0x4EC  starts a track upload (see handle-upload-begin)
;   Music_Upload_Chunk    0x4ED  one chunk of a track upload (up to 7 bytes)
;   Music_Upload_End      0x4EE  ends a track upload, length+checksum validated
;
; N on Music_Control_ID1 is checked against resident-song-number (or Axel F's
; reserved number); a command for any other number is ignored and logged.
;
; Play/stop/volume (and the 0x4EF full-status request) echo playback state on
; Music_Status (0x7E6):
;   byte0: play state (0 = stopped, 1 = playing)
;   byte1: current song number (0 if stopped)
;   byte2: master volume, 0..100 %
;   byte3: start counter, +1 (mod 256) on every song start, so the MDB can
;          tell a start from a volume reply; restarts at 0 on every boot
;   byte4: why the last song ended: 0 still playing / none, 1 stopped,
;          2 finished
; It is also sent when a song finishes on its own, and when an upload stops a
; playing song.
;
; Upload completion echoes on Music_Upload_Status (0x7E7):
;   byte0: song number, byte1: track slot, byte2: result
;     (0 OK, 1 checksum mismatch, 2 length mismatch, 3 rejected)
;   byte3: reserved
(def music-master-vol 1.0)   ; 0.0-1.0 global multiplier, set live via 0x4E9
(def music-start-count 0)    ; echoed on 0x7E6 byte3
(def music-end-reason 0)     ; echoed on 0x7E6 byte4: 0 none, 1 stopped, 2 finished

; Build and send ECU_status4 (0x7E3) reflecting current KERS state
(defun send-status4 (ebs-en gear-en) {
    (if (and boost-runtime-ready (ext-boost-report)) nil {
    (var status-byte 0)
    (setq status-byte (bitwise-or status-byte 0x01))           ; ecu_is_enabled = 1
                                                                 ; ecu_is_disabled = 0 (bit 1)
                                                                 ; boost_mode_enabled = 0 (bit 2)
    (setq status-byte (bitwise-or status-byte 0x08))            ; boost_mode_disabled = 1
    (if (= gear-en 1)
        (setq status-byte (bitwise-or status-byte 0x10))        ; gear_mode_enabled
        (setq status-byte (bitwise-or status-byte 0x20))        ; gear_mode_disabled
    )
    (if (= ebs-en 1)
        (setq status-byte (bitwise-or status-byte 0x40))        ; ebs_enabled
        (setq status-byte (bitwise-or status-byte 0x80))        ; ebs_disabled
    )
    (bufset-u8 dataArray_0x7E3 0 status-byte)
    (can-send-sid 0x7E3 dataArray_0x7E3)
    })
})

; Send ECU_ebs_get (0x7E5) echoing back current KERS parameters
(defun send-ebs-get () {
    (can-send-sid 0x7E5 dataArray_0x7E5)
})

@const-start
; Send Music_Status (0x7E6) echoing back current playback state.
;   byte 0: play state (0 = stopped, 1 = playing)
;   byte 1: current song number (0 if stopped)
;   byte 2: master volume, 0..100 %
;   byte 3: start counter, byte 4: end reason (see the protocol notes above)
(defun send-music-status () {
    (bufset-u8 dataArray_0x7E6 0 (if music-playing 1 0))
    (bufset-u8 dataArray_0x7E6 1 (if music-playing current-song 0))
    (bufset-u8 dataArray_0x7E6 2 (to-i (* music-master-vol 100.0)))
    (bufset-u8 dataArray_0x7E6 3 music-start-count)
    (bufset-u8 dataArray_0x7E6 4 music-end-reason)
    (can-send-sid 0x7E6 dataArray_0x7E6)
})

; Send Music_Upload_Status (0x7E7) echoing the result of an upload.
;   byte 0: song number, byte 1: track slot, byte 2: result
;     (0 OK, 1 checksum mismatch, 2 length mismatch, 3 rejected)
;   byte 3: reserved
(defun send-upload-status (song-number track-slot result) {
    (bufset-u8 dataArray_0x7E7 0 song-number)
    (bufset-u8 dataArray_0x7E7 1 track-slot)
    (bufset-u8 dataArray_0x7E7 2 result)
    (bufset-u8 dataArray_0x7E7 3 0)
    (can-send-sid 0x7E7 dataArray_0x7E7)
})
@const-end

; Map a VESC fault code onto the 0x7E1 numbering, which is the same number as
; the dashboard LED blink code. Unmapped faults collapse to 16, internal 15V.
(defun map-vesc-fault (v)
    (cond
        ((= v 0) 0)
        ((= v 1) 1)   ; over-voltage
        ((= v 2) 2)   ; under-voltage
        ((= v 3) 6)   ; DRV / gate driver -> MOSFET
        ((= v 4) 4)   ; abs over-current -> motor stalled
        ((= v 5) 11)  ; over-temp FET -> controller over-heat
        ((= v 6) 13)  ; over-temp motor -> motor temperature protection
        (t 16)
    )
)

; Send ECU_status2 (0x7E1): temperature + fault code.
;   byte 0   = ECU/FET temperature (°C, i8)
;   byte 1   = reserved
;   bytes 2-5 = fault code (u32, big-endian)
(defun send-status2 () {
    (bufset-i8 dataArray_0x7E1 0 (to-i (get-temp-fet)))
    (bufset-u8 dataArray_0x7E1 1 0)
    (bufset-u32 dataArray_0x7E1 2 (map-vesc-fault (get-fault)))
    (can-send-sid 0x7E1 dataArray_0x7E1)
})

; Send ECU_status5 (0x7E8): firmware identification block.
;   bytes 0-3 = warranty date, unused
;   bytes 4-7 = see the BCD constants above
(defun send-firmware-id () {
    (bufset-u32 dataArray_0x7E8 0 0)
    (bufset-u8 dataArray_0x7E8 4 motor-power-kw)
    (bufset-u8 dataArray_0x7E8 5 max-speed-kmh)
    (bufset-u8 dataArray_0x7E8 6 sw-base-version)
    (bufset-u8 dataArray_0x7E8 7 sw-app-version)
    (can-send-sid 0x7E8 dataArray_0x7E8)
})

; Send the ECU configuration block (0x7E9-0x7EF) from the live motor config.
; 10mV / 10mA units on the wire. conf-get has to be called from here, not at
; script load: these names resolve fine at runtime, but a top-level read errors.
(defun send-config-block () {
    (bufset-u16 dataArray_0x7E9 0 (to-i (* (conf-get 'l-max-vin) 100)))
    (can-send-sid 0x7E9 dataArray_0x7E9)
    (sleep 0.005)

    (bufset-u16 dataArray_0x7EA 0 (to-i (* (conf-get 'l-min-vin) 100)))
    (can-send-sid 0x7EA dataArray_0x7EA)
    (sleep 0.005)

    ; No VESC equivalent, report 100% so nothing infers a derate.
    (bufset-u8 dataArray_0x7EB 0 100)
    (can-send-sid 0x7EB dataArray_0x7EB)
    (sleep 0.005)

    ; Rolling circumference in cm, with the same 1.03 calibration send_stats
    ; applies to the reported km/h.
    (bufset-u8 dataArray_0x7EC 0
        (to-i (/ (* (conf-get 'si-wheel-diameter) 3.14159 100) speed-cal)))
    (can-send-sid 0x7EC dataArray_0x7EC)
    (sleep 0.005)

    (bufset-u16 dataArray_0x7EE 0 (to-i (* (conf-get 'l-current-max) 100)))
    (can-send-sid 0x7EE dataArray_0x7EE)
    (sleep 0.005)

    ; No separate startup current on VESC, mirror the peak limit.
    (bufset-u16 dataArray_0x7EF 0 (to-i (* (conf-get 'l-current-max) 100)))
    (can-send-sid 0x7EF dataArray_0x7EF)
    (sleep 0.005)
})

; ===== Music: runtime state (RAM) =====
(def num-chan 4)
; current-song is the stop/switch signal AND the number echoed on 0x7E6:
;   0     = stopped  — track threads exit their loops when they see this
;   N>0   = playing song N; generation tokens also distinguish same-song replays
(def music-playing false)
(def current-song 0)
(def music-t-start (systime))
(def music-play-generation 0)
(def music-active-tracks 0)
(def music-starting false)
(def music-upload-last -100.0)

; ===== Engine sound: runtime state (RAM) =====
; An engine hum on the tone channels that follows speed and throttle, in one of
; several styles (engine-set-style), with a start sound when it switches on and
; a stop sound when it switches off. Off at boot, switched with Engine_Sound
; (0x4EA). Music has priority.
(def engine-enabled false)
(def engine-running false)    ; started (start sound played), not stopped yet
(def engine-vol-pct 50)       ; volume as set over CAN, echoed on 0x7ED
(def engine-vol 1.0)          ; tone voltage at full load (engine-vol-pct of max)
(def engine-style 0)          ; 0 v8, 1 moped, 2 sport, 3 vtwin, 4 electric
(def engine-p nil)            ; the current style's parameters (engine-style-table)
; Sound state.
(def engine-rpm 70.0)         ; smoothed base tone
(def engine-prev-target 70.0)
(def engine-load 0.35)        ; smoothed loudness factor 0..1
(def engine-shift 0)          ; ticks left in the clutch dip after an upshift
(def engine-beat-phase 0.0)
(def engine-rng 1)
(def engine-sounding false)   ; engine tones are on the channels right now
(def engine-demo false)       ; the demo ride is playing
(def engine-last-status 0.0)  ; Engine_Status is also repeated once per second

; Music functions live in flash (@const-start/@const-end), not the RAM cell
; heap: upstream alone already fills about half the heap, and the music code
; on top of it ran the heap out. Only function definitions go in const
; blocks — anything mutated at runtime (channels, flags, buffers) stays in RAM.
@const-start

; Engine tones share the four audio channels with MIDI and sampled playback.
; They stay quiet while a song plays or starts, for a moment after upload
; traffic, and while the controller reports a fault.
(defun engine-quiet ()
    (or music-playing music-starting (< (secs-since music-upload-last) 0.3)
        (!= (get-fault) 0)))

; Called when a song starts or an upload begins: release the channels at once.
(defun engine-pause () {
    (if engine-sounding (engine-silence-locked))
})

(defun engine-init () {
    (foc-play-stop)
    (def engine-enabled false)
    (def engine-running false)
    (def engine-demo false)
    (def engine-sounding false)
    (def engine-vol-pct 50)
    (def engine-vol (* 0.5 (engine-vol-max)))
    (engine-set-style 0)
    (engine-reset)
    (def engine-last-status (systime))
    (def music-play-generation 0)
    (def music-active-tracks 0)
    (def music-starting false)
    (def music-playing false)
    (def current-song 0)
    (def music-upload-last -100.0)
})

; Engine_Status (0x7ED): byte 0 engine sound on/off, byte 1 volume %,
; byte 2 demo ride playing, byte 3 style.
(defun send-engine-status-locked () {
    (bufset-u8 dataArray_0x7ED 0 (if engine-enabled 1 0))
    (bufset-u8 dataArray_0x7ED 1 engine-vol-pct)
    (bufset-u8 dataArray_0x7ED 2 (if engine-demo 1 0))
    (bufset-u8 dataArray_0x7ED 3 engine-style)
    (can-send-sid 0x7ED dataArray_0x7ED)
    (def engine-last-status (systime))
})

(defun send-engine-status () (atomic (send-engine-status-locked)))

; Engine_Sound (0x4EA): byte0 0 off (also stops the demo) / 1 on / 2 demo ride,
; byte1 (optional) volume %, byte2 (optional) style 0..4. A command with an
; unknown byte0 or style is ignored; a repeated on/off/demo changes nothing.
(defun handle-engine-command (data) {
    (var n (buflen data))
    (var cmd (if (> n 0) (bufget-u8 data 0) 255))
    (var style (if (> n 2) (bufget-u8 data 2) engine-style))
    (if (and (<= cmd 2) (<= style 4)) {
        (if (> n 1) {
            (var pct (bufget-u8 data 1))
            (if (> pct 100) (setq pct 100))
            (def engine-vol-pct pct)
            (def engine-vol (* (/ (to-float pct) 100.0) (engine-vol-max)))
        })
        (if (!= style engine-style) (engine-set-style style))
        (if (= cmd 2)
            (def engine-demo t)
            {
                (def engine-enabled (= cmd 1))
                (if (= cmd 0) (def engine-demo false))
            })
    })
    (send-engine-status)
})

; ===== Engine sound =====
; One model for every style: a base tone from speed through virtual gears
; (within each gear it climbs to engine-redline, an upshift drops it to
; engine-shift-start of the band), four tones at fixed ratios of it, and the
; style's character on top (an uneven firing beat, a raspy flutter). Each style
; has its own start and stop sequence, played at 10 ms steps.

; Style parameters, one constant list per style (in flash, so a style costs no
; heap): gears; idle and redline Hz; shift-start (0..1); idle-load (loudness at
; idle, 0..1); tones (ratio amp x4); beat divisor and gap (an uneven twin beat
; at base/divisor Hz, second pulse at gap of the cycle; 0: none); rasp (0..1,
; random flutter); start kind (0 starter, 1 kick, 2 power-up); crank secs,
; crank Hz from/to, crank strokes/s from/to (starter only); flare Hz (rev flare
; when it catches); stop kind (0 run-down, 1 rattle, 2 power-down); stop secs;
; Hz at the end of the run-down. Tone amplitudes are scaled so every style has
; the V8's total tone level (2.25 x engine-vol): none is louder or draws more
; current than another.
(defun engine-style-table (id)
    (cond
        ((= id 1)   ; moped: soft and low, harmonics 1-2-3 (from the Zanooda/tunu-configs moped), 4 gears, kick start
            '(4 60.0 250.0 0.55 0.45 (1.0 1.350916 2.0 0.674695 3.0 0.224389 1.0 0.0) 0.0 0.0 0.0 1 0.0 0.0 0.0 0.0 0.0 170.0 1 1.0 25.0))
        ((= id 2)   ; sport bike: inline four, very high revs, 6 gears
            '(6 90.0 400.0 0.62 0.3 (0.5 0.287234 1.0 0.670213 2.0 0.957447 4.0 0.335106) 0.0 0.0 0.0 0 1.0 250.0 330.0 7.0 9.0 230.0 0 1.0 35.0))
        ((= id 3)   ; V-twin cruiser: deep, uneven beat, 4 gears
            '(4 55.0 120.0 0.5 0.4 (1.0 0.9 1.5 0.54 2.0 0.36 0.5 0.45) 8.0 0.35 0.0 0 2.2 110.0 160.0 2.8 3.8 95.0 0 2.4 18.0))
        ((= id 4)   ; electric / sci-fi: no gears, smooth whine, power-up chime
            '(1 120.0 520.0 0.0 0.25 (1.0 1.184211 2.0 0.296053 3.01 0.177632 1.005 0.592105) 0.0 0.0 0.0 2 0.0 0.0 0.0 0.0 0.0 0.0 2 0.0 0.0))
        (t          ; V8: five gears, starter
            '(5 70.0 170.0 0.45 0.35 (1.0 1.0 1.5 0.5 2.0 0.35 1.01 0.4) 0.0 0.0 0.0 0 1.8 150.0 230.0 4.0 6.0 125.0 0 1.8 22.0))))

(defun engine-gears () (ix engine-p 0))
(defun engine-idle () (ix engine-p 1))
(defun engine-redline () (ix engine-p 2))
(defun engine-shift-start () (ix engine-p 3))
(defun engine-idle-load () (ix engine-p 4))
(defun engine-tones () (ix engine-p 5))
(defun engine-beat-div () (ix engine-p 6))
(defun engine-beat-gap () (ix engine-p 7))
(defun engine-rasp () (ix engine-p 8))
(defun engine-start-kind () (ix engine-p 9))
(defun engine-crank () (ix engine-p 10))
(defun engine-crank-lo () (ix engine-p 11))
(defun engine-crank-hi () (ix engine-p 12))
(defun engine-crank-r0 () (ix engine-p 13))
(defun engine-crank-r1 () (ix engine-p 14))
(defun engine-flare () (ix engine-p 15))
(defun engine-stop-kind () (ix engine-p 16))
(defun engine-stop-secs () (ix engine-p 17))
(defun engine-stop-to () (ix engine-p 18))

; Hard cap for engine-vol (100 %). The engine tones sit low (70-170 Hz), where
; the winding's impedance is lowest, so the same voltage drives more current
; than music does; all of it is heat, none torque.
; WARNING: don't raise this cap, and don't run the engine sound much above the
; default 50 % for long: the extra current heats the motor windings and can
; physically damage the motor.
(defun engine-vol-max () 2.0)
(defun engine-top-kmh () 46.0)

(defun engine-set-style (id) {
    (if (or (< id 0) (> id 4)) (setq id 0))
    (def engine-p (engine-style-table id))
    (def engine-style id)
})

(defun engine-target-rpm (kmh) {
    (var step (/ (engine-top-kmh) (engine-gears)))
    (var gear (to-i (/ kmh step)))
    (if (>= gear (engine-gears)) (setq gear (- (engine-gears) 1)))
    (var frac (/ (- kmh (* gear step)) step))
    (if (> frac 1.0) (setq frac 1.0))
    (var start (if (> gear 0) (engine-shift-start) 0.0))
    (+ (engine-idle) (* (- (engine-redline) (engine-idle)) (+ start (* (- 1.0 start) frac))))
})

; Silence the engine tones on every channel the music isn't using.
(defun engine-silence-locked () {
    (loopforeach ch channels
        (if (= (ix ch 1) 0) (foc-play-tone (ix ch 0) 500 0)))
    (def engine-sounding false)
})

(defun engine-silence () (atomic (engine-silence-locked)))

; Start from idle, e.g. when the sound comes back on.
(defun engine-reset () {
    (def engine-rpm (engine-idle))
    (def engine-prev-target (engine-idle))
    (def engine-load (engine-idle-load))
    (def engine-shift 0)
    (def engine-beat-phase 0.0)
})

; Pseudo-random 0..1 for the rasp (a small LCG: cheap, no allocation).
(defun engine-rand () {
    (def engine-rng (mod (+ (* engine-rng 75) 74) 65537))
    (/ (to-float engine-rng) 65537.0)
})

; Put the style's four tones on the channels for base frequency f (Hz) and
; loudness ld (0..1 of engine-vol), with the style's character. dt is the time
; since the last call, for the firing beat.
(defun engine-out (f ld dt) {
    (var a (* engine-vol ld))
    (if (> (engine-beat-div) 0.0) {
        (var x (+ engine-beat-phase (* (/ f (engine-beat-div)) dt)))
        (if (>= x 1.0) (setq x (- x 1.0)))
        (def engine-beat-phase x)
        (if (not (or (< x 0.2) (and (>= x (engine-beat-gap)) (< x (+ (engine-beat-gap) 0.2)))))
            (setq a (* a 0.35)))
    })
    (if (> (engine-rasp) 0.0) (setq a (* a (- 1.0 (* (engine-rasp) (engine-rand))))))
    (var tn (engine-tones))
    (atomic (if (not (engine-quiet)) {
        (def engine-sounding t)
        (foc-play-tone 0 (* f (ix tn 0)) (* a (ix tn 1)))
        (foc-play-tone 1 (* f (ix tn 2)) (* a (ix tn 3)))
        (foc-play-tone 2 (* f (ix tn 4)) (* a (ix tn 5)))
        (foc-play-tone 3 (* f (ix tn 6)) (* a (ix tn 7)))
    }))
})

; Explicit tones for sequence sounds that aren't the engine's tone set (starter
; whine and thud, chime, sweep). Amplitudes are fractions of engine-vol.
(defun engine-raw (f0 a0 f1 a1 f2 a2 f3 a3)
    (atomic (if (not (engine-quiet)) {
        (def engine-sounding t)
        (foc-play-tone 0 f0 (* engine-vol a0))
        (foc-play-tone 1 f1 (* engine-vol a1))
        (foc-play-tone 2 f2 (* engine-vol a2))
        (foc-play-tone 3 f3 (* engine-vol a3))
    })))

; Can a sequence step run? mode 0: start (while switched on), 1: stop (while
; switched off), 2: demo (while the demo runs). A song, an upload or a fault
; always wins.
(defun engine-report-if-due ()
    (if (>= (secs-since engine-last-status) 1.0) (send-engine-status)))

(defun engine-seq-ok (mode) {
    (engine-report-if-due)
    (and (not (engine-quiet))
         (cond ((= mode 0) (and engine-enabled (not engine-demo)))
               ((= mode 1) (and (not engine-enabled) (not engine-demo)))
               (t engine-demo)))
})

(defun engine-in (x lst) {
    (var found nil)
    (loopforeach el lst (if (= el x) { (setq found t) (break) }))
    found
})

; The engine catching: revs flare from f towards (engine-flare) (with misfires
; at the given steps), then settle to idle.
(defun engine-catch (mode f k steps misfires settle-n settle-k load-k) {
    (var ok t)
    (looprange i 0 steps {
        (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
        (setq f (+ f (* (- (engine-flare) f) k)))
        (engine-out f (if (engine-in i misfires) 0.15 1.0) 0.01)
        (sleep 0.01)
    })
    (var ld 1.0)
    (if ok (looprange i 0 settle-n {
        (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
        (setq f (+ f (* (- (engine-idle) f) settle-k)))
        (setq ld (+ ld (* (- (engine-idle-load) ld) load-k)))
        (engine-out f ld 0.01)
        (sleep 0.01)
    }))
    ok
})

; Starter: a whine rising over the crank, pulsing with the compression strokes,
; a low thud on each; then the engine catches.
(defun engine-start-starter (mode) {
    (var ok t)
    (looprange i 0 (to-i (/ (engine-crank) 0.01)) {
        (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
        (var tt (* i 0.01))
        (var rate (+ (engine-crank-r0) (* (- (engine-crank-r1) (engine-crank-r0)) (/ tt (engine-crank)))))
        (var ph (* tt rate))
        (setq ph (- ph (to-float (to-i ph))))
        (var pulse (+ 0.55 (* 0.45 (abs (sin (* 3.14159 ph))))))
        (var fs (+ (engine-crank-lo) (* (- (engine-crank-hi) (engine-crank-lo)) (/ tt (engine-crank)))))
        (var thud (if (< ph 0.12) 0.9 0.0))
        (engine-raw fs (* 0.45 pulse) (* fs 2.02) (* 0.15 pulse) 42.0 thud 63.0 (* 0.4 thud))
        (sleep 0.01)
    })
    (if ok (engine-catch mode (* (engine-idle) 0.6) 0.13 50 '(3 8 14 21) 120 0.045 0.04) ok)
})

; Kick start: one kick that dies, a pause, a second kick that catches.
(defun engine-start-kick (mode) {
    (var ok t)
    (looprange kick 0 2 {
        (looprange i 0 30 {
            (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
            (var x (/ (to-float i) 30.0))
            (var y (* x 1.6))
            (engine-out (+ 50.0 (* 110.0 (sin (* 3.14159 (if (> y 1.0) 1.0 y))))) (+ 0.1 (* 0.8 (- 1.0 x))) 0.01)
            (sleep 0.01)
        })
        (if (not ok) (break))
        (if (= kick 0) { (engine-silence) (sleep 0.45) })
    })
    (if ok (engine-catch mode 150.0 0.15 45 '(4 10 17) 100 0.05 0.05) ok)
})

; Power-up: a rising sweep, a two-tone chime, then down to the idle hum.
(defun engine-start-powerup (mode) {
    (var ok t)
    (looprange i 0 60 {
        (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
        (var x (/ (to-float i) 60.0))
        (var f (+ 200.0 (* 700.0 x x)))
        (engine-raw f (+ 0.1 (* 0.6 x)) (* f 1.5) (* 0.2 x) 500.0 0.0 500.0 0.0)
        (sleep 0.01)
    })
    (loopforeach nt '((880.0 12) (1320.0 22)) {
        (var amp 0.8)
        (if ok (looprange i 0 (ix nt 1) {
            (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
            (engine-raw (ix nt 0) amp (* (ix nt 0) 2.0) (* amp 0.25) 500.0 0.0 500.0 0.0)
            (setq amp (* amp 0.97))
            (sleep 0.01)
        }))
    })
    (var f 600.0)
    (var ld 0.6)
    (if ok (looprange i 0 80 {
        (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
        (setq f (+ f (* (- (engine-idle) f) 0.06)))
        (setq ld (+ ld (* (- (engine-idle-load) ld) 0.06)))
        (engine-out f ld 0.01)
        (sleep 0.01)
    }))
    ok
})

; Start sound of the current style. Returns nil if it was cut short.
(defun engine-start-seq (mode) {
    (var ok (cond
        ((= (engine-start-kind) 1) (engine-start-kick mode))
        ((= (engine-start-kind) 2) (engine-start-powerup mode))
        (t (engine-start-starter mode))))
    (engine-reset)
    ok
})

; Stop sound of the current style: the revs run down and get rougher (a
; rattle for the moped) and end in a thump, or a power-down sweep.
(defun engine-stop-seq (mode) {
    (var ok t)
    (if (= (engine-stop-kind) 2)
        (looprange i 0 100 {
            (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
            (var x (/ (to-float i) 100.0))
            (var f (+ (* 450.0 (- 1.0 x)) (* 50.0 x)))
            (engine-raw f (* 0.6 (- 1.0 x)) (* f 1.5) (* 0.2 (- 1.0 x)) 500.0 0.0 500.0 0.0)
            (sleep 0.01)
        })
        {
            (var n (to-i (/ (engine-stop-secs) 0.01)))
            (looprange i 0 n {
                (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
                (var x (/ (to-float i) (to-float n)))
                (var y (- 1.0 x))
                (var f (- (engine-idle) (* (- (engine-idle) (engine-stop-to)) (- 1.0 (* y y)))))
                (var rough (+ 0.15 (* 0.6 x)))
                (var flutter (if (= (engine-stop-kind) 1)
                    (- 1.0 (* rough (engine-rand)))
                    (- 1.0 (* rough (+ 0.5 (* 0.5 (sin (* 6.28318 (/ f 18.0) (to-float i) 0.06))))))))
                (engine-out f (* (engine-idle-load) (- 1.0 (* 0.5 x)) flutter) 0.01)
                (sleep 0.01)
            })
            (var amp 0.9)
            (if ok (looprange i 0 9 {
                (if (not (engine-seq-ok mode)) { (setq ok nil) (break) })
                (engine-raw 38.0 amp 57.0 (/ amp 3.0) 500.0 0.0 500.0 0.0)
                (setq amp (* amp 0.7))
                (sleep 0.01)
            }))
        })
    (engine-silence)
    ok
})

; One 20 ms step of the sound for a speed (km/h), throttle and brake (0..1).
; Standing still with the brake pressed, the throttle revs the engine freely,
; as if in neutral: up quickly, down slowly like a flywheel. Sound only: the
; ADC app's own brake handling keeps the motor still.
(defun engine-tick (kmh thr brk) {
    (var want (engine-idle-load))
    (if (and (< kmh 1.0) (> brk 0.3)) {
        (var target (+ (engine-idle) (* (- (engine-redline) (engine-idle)) 0.95 thr)))
        (def engine-prev-target (engine-idle))
        (def engine-shift 0)
        (def engine-rpm (+ engine-rpm (* (- target engine-rpm) (if (> target engine-rpm) 0.3 0.06))))
        (setq want (+ (engine-idle-load) (* (- 1.0 (engine-idle-load)) thr)))
    } {
        (var net (- thr brk))
        (if (< net 0.0) (setq net 0.0))
        (var target (engine-target-rpm kmh))
        ; A sudden drop of the target is an upshift: dip the load like a clutch.
        (if (< target (* engine-prev-target 0.9)) (def engine-shift 9))
        (def engine-prev-target target)
        ; Revs fall over ~0.15 s on an upshift instead of jumping.
        (def engine-rpm (+ engine-rpm (* (- target engine-rpm) (if (< target engine-rpm) 0.13 0.35))))
        (setq want (+ (engine-idle-load) (* (- 1.0 (engine-idle-load)) net)))
        (if (> engine-shift 0) {
            (setq want (* want 0.35))
            (def engine-shift (- engine-shift 1))
        })
    })
    (def engine-load (+ engine-load (* (- want engine-load) 0.2)))
    (engine-out engine-rpm engine-load 0.02)
})

(defun engine-clamp (x) (if (< x 0.0) 0.0 (if (> x 1.0) 1.0 x)))

; 50 Hz loop. Switching on plays the start sound, switching off the stop sound.
; While a song plays (or starts, or uploads) or the controller reports a fault,
; the engine stays silent and resumes without a new start sound; the music's
; own note-on/decay-loop own the channels then. Start, stop and demo sequences
; all run on this worker, so only one renderer owns the tones and model state.
; Engine_Status is repeated once per second, including during sequences.
(defun engine-step () {
    (cond
        (engine-demo (engine-demo-run))
        ((and engine-enabled (not (engine-quiet))) {
            (if (not engine-running) {
                (def engine-running t)
                (engine-start-seq 0)
            })
            (atomic (if (or engine-demo (engine-quiet)) (engine-pause)))
            (if (not engine-sounding) (engine-reset))
            (if (and engine-enabled (not engine-demo) (not (engine-quiet))) {
                (engine-tick (/ (* (abs (get-speed)) 3.6) speed-cal)
                             (engine-clamp (get-adc-decoded 0))
                             (engine-clamp (get-adc-decoded 1)))
            })
        })
        ((and (not engine-enabled) engine-running) {
            (if (not (engine-quiet)) (engine-stop-seq 1))
            (def engine-running nil)
            (engine-silence)
            (def engine-sounding nil)
        })
        (engine-sounding {
            (engine-silence)
            (def engine-sounding nil)
        }))
    (engine-report-if-due)
})

; If the loop ever stops with an error, it releases the channels and reports off.
(defun engine-loop () {
    (var result (trap (loopwhile t { (engine-step) (sleep 0.02) })))
    (atomic {
        (engine-pause)
        (def engine-enabled false)
        (def engine-running false)
        (def engine-demo false)
    })
    (send-engine-status)
    (print (list "Engine sound stopped" result))
})

; One segment of the demo ride: move the simulated speed towards target-kmh at
; accel km/h per second for secs seconds, with the throttle at thr. Returns the
; speed reached. Ends early when the demo is stopped or a song starts.
(defun engine-demo-seg (kmh secs target-kmh accel thr) {
    (var step (* accel 0.02))
    (looprange i 0 (to-i (* secs 50.0))
        (if (engine-seq-ok 2) {
            (var d (- target-kmh kmh))
            (setq kmh (+ kmh (if (> d step) step (if (< d (- 0.0 step)) (- 0.0 step) d))))
            (engine-tick kmh thr 0.0)
            (sleep 0.02)
        } (break)))
    kmh
})

; Demo, ~30 s: start sound, idle, full throttle to 45 km/h, cruise, roll off,
; brake to a stop, idle, stop sound. Same sound code as a real ride.
(defun engine-demo-ride () {
    (engine-reset)
    (def engine-demo t)
    (send-engine-status)
    (if (engine-start-seq 2) {
        (var v 0.0)
        (setq v (engine-demo-seg v 2.0 0.0 5.0 0.0))
        (setq v (engine-demo-seg v 9.0 45.0 6.0 1.0))
        (setq v (engine-demo-seg v 4.0 45.0 2.0 0.35))
        (setq v (engine-demo-seg v 3.0 30.0 5.0 0.0))
        (setq v (engine-demo-seg v 3.0 0.0 14.0 0.0))
        (engine-demo-seg v 2.0 0.0 5.0 0.0)
        (if (engine-seq-ok 2) (engine-stop-seq 2))
    })
})

(defun engine-demo-run () {
    (trap (engine-demo-ride))
    (def engine-demo false)
    (engine-silence)
    (def engine-sounding false)
    (send-engine-status)
})

(defun advance-music-generation ()
    (atomic {
        (def music-play-generation (+ music-play-generation 1))
        music-play-generation
    }))

(defun music-generation-current (generation)
    (= generation music-play-generation))

(defun music-track-finished (generation parser-idx)
    (atomic (if (= generation music-play-generation) {
        (var bit (shl 1 parser-idx))
        (if (!= (bitwise-and music-active-tracks bit) 0)
            (def music-active-tracks (- music-active-tracks bit)))
        (if (= music-active-tracks 0) {
            (def music-play-generation (+ music-play-generation 1))
            (def current-song 0)
            (def music-playing false)
            (def music-starting false)
            (def music-end-reason 2)
            (foc-play-stop)
            (loopforeach ch channels { (setix ch 1 0) })
            (send-music-status)
        })
    })))

; ===== Music: polyphony engine =====

; Scale a track volume by MIDI note velocity. velocity 100 = full volume;
; lower velocities attenuate individual notes.
(defun vscale (vol vel) {
    (var f (/ (to-float vel) 100.0))
    (* vol (if (> f 1.0) 1.0 f))
})

(defun note-on (note vol decay generation)
    (atomic (loopforeach ch channels {
        (if (and (music-generation-current generation) (= (ix ch 1) 0)) {
            (var freq (* 440.0 (pow 2.0 (/ (- note 69.0) 12.0))))
            (var v (* vol music-master-vol))   ; apply live master volume
            (setix ch 2 freq)
            (setix ch 3 v)
            (setix ch 4 decay)
            (setix ch 1 note)
            (foc-play-tone (ix ch 0) freq v)
            (break)
        })
    })))

(defun note-off (note generation)
    (atomic (loopforeach ch channels {
        (if (and (music-generation-current generation) (= (ix ch 1) note)) {
            (setix ch 1 0)
            (foc-play-tone (ix ch 0) 500 0)
            (break)
        })
    })))

; Exponential volume decay thread — sustain envelope for active notes
(defun decay-loop ()
    (loopwhile t {
        (loopforeach ch channels
            (atomic (if (not (= (ix ch 1) 0)) {
                (setix ch 3 (* (ix ch 3) (ix ch 4)))
                (foc-play-tone (ix ch 0) (ix ch 2) (ix ch 3))
            })))
        (sleep 0.02)
    }))

; ===== Music: playback =====

; The song and generation are captured at spawn time; both must still own playback.
; Streamed songs play from file time 0, so all tracks share one time zero and a
; track that enters later stays in time. trim skips this track's own lead-in
; instead, for the package-imported Axel F.
(defun play-midi-track (parser-idx volts decay song-id generation trim) {
    (var t-delta 0.0)
    (var midi-tdiv 100)
    (var midi-tempo 400000)
    (var t-offset (if trim -1.0 0.0))  ; -1: set on the first event
    (loopwhile (and (= current-song song-id) (music-generation-current generation))
        (match (atomic (if (music-generation-current generation) (ext-midi-parse parser-idx) 'midi-eob))
            ((midi-header (? size) (? format) (? tracks) (? time-div)) {
                (setq midi-tdiv time-div)
            })
            ((midi-track-midi (? time) (? status) (? chan) (? p1) (? p2)) {
                (var secs (/ (* midi-tempo (to-float time)) midi-tdiv 1000000.0))
                (setq t-delta (+ t-delta secs))
                ; On first event, anchor t-offset so the track starts immediately
                (if (= t-offset -1.0) (setq t-offset t-delta))
                (var to-sleep (- (- t-delta t-offset) (secs-since music-t-start)))
                ; Chunked sleep: wake every 50ms to check if song was stopped/changed.
                ; Prevents a long inter-note gap from delaying the stop response.
                (loopwhile (and (> to-sleep 0.05) (= current-song song-id) (music-generation-current generation)) {
                    (sleep 0.05)
                    (setq to-sleep (- to-sleep 0.05))
                })
                (if (and (> to-sleep 0.001) (= current-song song-id) (music-generation-current generation)) (sleep to-sleep))
                (if (not (music-generation-current generation)) (break))
                (if (= status 9) (note-on p1 (vscale volts p2) decay generation))
                (if (= status 8) (note-off p1 generation))
            })
            ((midi-track-meta (? time) (? type) (? len) (? data)) {
                (if (= type 81) (setq midi-tempo data))
                ; End of Track: stop here. The song buffers are reused, so
                ; past the end of a shorter track lie the old song's bytes.
                (if (= type 47) (break))
            })
            (midi-error { (print "MIDI error") (break) })
            (midi-eob (break))
            (_ nil)   ; other events (track headers etc.) need no handling
        )
    )
    (music-track-finished generation parser-idx)
})

(defun stop-music () {
    (var generation (advance-music-generation))
    (atomic (if (music-generation-current generation) {
        (if music-playing (def music-end-reason 1))
        (def current-song 0)
        (def music-playing false)
        (def music-starting false)
        (def music-active-tracks 0)
        (foc-play-stop)
        (loopforeach ch channels { (setix ch 1 0) })
    }))
})

; @private-warp-functions

; Plays whatever's currently loaded in song-buf-0/1, according to which track
; slots handle-upload-end has marked ready.
(defun play-current-song () {
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
    (atomic (if (music-generation-current generation) {
        (if (or song-buf-0-ready song-buf-1-ready) {
            (if song-buf-0-ready (ext-midi-open 0 song-buf-0))
            (if song-buf-1-ready (ext-midi-open 1 song-buf-1))
            (def music-active-tracks (+ (if song-buf-0-ready 1 0) (if song-buf-1-ready 2 0)))
            (def music-t-start (systime))
            (def current-song resident-song-number)
            (def music-playing true)
            (def music-start-count (mod (+ music-start-count 1) 256))
            (def music-end-reason 0)
            (if song-buf-0-ready (spawn 200 play-midi-track 0 song-buf-0-vol song-buf-0-decay resident-song-number generation false))
            (if song-buf-1-ready (spawn 200 play-midi-track 1 song-buf-1-vol song-buf-1-decay resident-song-number generation false))
        } {
            (print "No song buffer ready to play")
        })
        (def music-starting false)
    }))
})

; Axel F bypasses the streamed-buffer model — see the import comment at the
; top. Same shape as play-current-song, hardcoded to the package buffers.
(defun play-axelf () {
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
    (atomic (if (music-generation-current generation) {
        (ext-midi-open 0 song1-track0)
        (ext-midi-open 1 song1-track1)
        (def music-active-tracks 3)
        (def music-t-start (systime))
        (def current-song axelf-song-number)
        (def music-playing true)
        (def music-starting false)
        (def music-start-count (mod (+ music-start-count 1) 256))
        (def music-end-reason 0)
        (spawn 200 play-midi-track 0 2 0.92 axelf-song-number generation true)
        (spawn 200 play-midi-track 1 1.0 0.92 axelf-song-number generation true)
    }))
})

; ===== Music: song upload (streaming) =====
; Music_Upload_Begin (0x4EC) — starts a new track upload.
;   byte0 = song number, byte1 = track slot (0 melody/1 bass),
;   byte2-3 = length LE u16, byte4 = volume x10, byte5 = decay x100
(defun handle-upload-begin (data) {
    (def music-upload-last (systime))
    (engine-pause)
    (var song-number (bufget-u8 data 0))
    (var track-slot  (bufget-u8 data 1))
    (var track-len (+ (bufget-u8 data 2) (* (bufget-u8 data 3) 256)))
    (var vol-x10 (bufget-u8 data 4))
    (var decay-x100 (bufget-u8 data 5))
    (if (or (> track-slot 1) (> track-len song-buf-max)) {
        (print (list "upload begin rejected: slot" track-slot "len" track-len))
        (send-upload-status song-number track-slot 3)
    } {
        ; The buffers about to be refilled may be mid-playback — stop first,
        ; or a play-midi-track thread would read them while they're rewritten.
        ; Report it, so the MDB sees the song end (stopped).
        (if (or music-playing music-starting) {
            (stop-music)
            (send-music-status)
        })
        ; A new song number clears BOTH track-ready flags, so a mono song
        ; doesn't inherit a stale second track from a previous song.
        (if (!= song-number resident-song-number) {
            (def song-buf-0-ready false)
            (def song-buf-1-ready false)
        })
        (def upload-active-slot track-slot)
        (def upload-checksum 0)
        (def upload-received 0)
        (if (= track-slot 0) {
            (def song-buf-0-len track-len)
            (def song-buf-0-vol (/ (to-float vol-x10) 10.0))
            (def song-buf-0-decay (/ (to-float decay-x100) 100.0))
            (def song-buf-0-ready false)
        } {
            (def song-buf-1-len track-len)
            (def song-buf-1-vol (/ (to-float vol-x10) 10.0))
            (def song-buf-1-decay (/ (to-float decay-x100) 100.0))
            (def song-buf-1-ready false)
        })
        (def resident-song-number song-number)
    })
})

; Music_Upload_Chunk (0x4ED) — byte0 = sequence (informational only; chunks
; are appended in arrival order), byte1-7 = up to 7 bytes.
(defun handle-upload-chunk (data) {
    (if (and (>= upload-active-slot 0) (> (buflen data) 1)) {
        (def music-upload-last (systime))
        (var n (- (buflen data) 1))
        (var dst (if (= upload-active-slot 0) song-buf-0 song-buf-1))
        (looprange i 0 n {
            (var b (bufget-u8 data (+ i 1)))
            (if (< (+ upload-received i) song-buf-max) {
                (bufset-u8 dst (+ upload-received i) b)
                (def upload-checksum (mod (+ upload-checksum b) 256))
            })
        })
        (def upload-received (+ upload-received n))
    })
})

; Music_Upload_End (0x4EE) — byte0 = song number, byte1 = track slot,
; byte2 = checksum, byte3 = reserved. Validates length+checksum and, on
; success, marks that track slot ready to play. On failure music-service
; retries the whole track from Begin.
(defun handle-upload-end (data) {
    (def music-upload-last (systime))
    (var song-number (bufget-u8 data 0))
    (var track-slot  (bufget-u8 data 1))
    (var checksum    (bufget-u8 data 2))
    (var expected-len (if (= track-slot 0) song-buf-0-len song-buf-1-len))
    (cond
        ((< upload-active-slot 0) (send-upload-status song-number track-slot 3))
        ; Must match the slot Begin opened, or a stale/reordered End could
        ; mark the wrong slot ready using leftover length/checksum.
        ((!= track-slot upload-active-slot) {
            (print (list "upload end: slot mismatch, expected" upload-active-slot "got" track-slot))
            (send-upload-status song-number track-slot 3)
        })
        ((!= upload-received expected-len) {
            (print (list "upload end: length mismatch" upload-received "!=" expected-len))
            (send-upload-status song-number track-slot 2)
        })
        ((!= upload-checksum checksum) {
            (print (list "upload end: checksum mismatch" upload-checksum "!=" checksum))
            (send-upload-status song-number track-slot 1)
        })
        (t {
            (if (= track-slot 0) (def song-buf-0-ready true) (def song-buf-1-ready true))
            (def upload-active-slot -1)
            (send-upload-status song-number track-slot 0)
        })
    )
})

; Music CAN frames, called from proc-sid. data is freed by proc-sid.
(defun proc-music (id data) {
    ; Music_Control_ID1 (0x4E8) - play/stop
    ; 4E8#00 = stop, 4E8#<N> = play song N (N = resident-song-number or
    ; axelf-song-number; anything else is ignored and logged)
    (if (= id 0x4E8u32) {
        (var cmd (bufget-u8 data 0))
        (cond
            ((= cmd 0x00) (stop-music))
            ((= cmd axelf-song-number) (play-axelf))
            ; @private-warp-dispatch
            ((= cmd resident-song-number) (play-current-song))
            (t (print (list "Music cmd" cmd "ignored: no matching resident song"))))
        (send-music-status)
    })

    (if (= id 0x4EAu32) (handle-engine-command data))

    ; Music_Control_ID2 (0x4E9) - master volume
    ; 4E9#<0..100 in hex>  (e.g. 4E9#50 = 80%)
    (if (= id 0x4E9u32) {
        (var vol-pct (bufget-u8 data 0))
        (if (> vol-pct 100) (setq vol-pct 100))   ; clamp
        (def music-master-vol (/ (to-float vol-pct) 100.0))
        (send-music-status)
    })

    ; Music_Upload_Begin (0x4EC) / Chunk (0x4ED) / End (0x4EE)
    (if (= id 0x4ECu32) (handle-upload-begin data))
    (if (= id 0x4EDu32) (handle-upload-chunk data))
    (if (= id 0x4EEu32) (handle-upload-end data))
})
@const-end

; Handler for SID CAN-frames
(defun proc-sid (id data) {

    ; Control_Message_ID4 (0x4E0) - KERS/boost/gear enable
    ; Byte 0: bit0=gear_mode_enable, bit1=boost_mode_enable, bit2=ebs_enable
    (if (= id 0x4E0u32) {
        (var ctrl-byte (bufget-u8 data 0))
        (var gear-en  (bitwise-and ctrl-byte 0x01))
        (var boost-en (shr (bitwise-and ctrl-byte 0x02) 1))
        (var ebs-en   (shr (bitwise-and ctrl-byte 0x04) 2))
        (def boost-control-byte (bitwise-and ctrl-byte 7))
        (boost-tick)

        (def kers-enabled (= ebs-en 1))
        (print (list "KERS:" kers-enabled "gear:" gear-en "boost:" boost-en))

        ; The battery-side regen limit (l-in-current-min, negative = regen
        ; direction, 0.0 = none allowed) is applied by the main loop so the
        ; voltage taper tracks the live pack voltage; only the state changes
        ; here.
        (print (list "Regen target:" (if kers-enabled "on" "off")))

        ; Report status4 back so the service sees the correct state
        (send-status4 ebs-en gear-en)
    })

    ; ECU_ebs_set (0x4E2) - KERS voltage/current parameters
    ; ecu-service sends values in 10mV/10mA units, multiply by 10 to get mV/mA
    (if (= id 0x4E2u32) {
        (var raw-voltage (bufget-u16 data 0))
        (var raw-current (bufget-u16 data 2))
        (def kers-voltage (* raw-voltage 10))
        (def kers-current (* raw-current 10))
        (print (list "KERS params V:" kers-voltage "mV, I:" kers-current "mA"))

        ; Echo the incoming values straight back on 0x7E5. Both the request
        ; (0x4E2) and the report (0x7E5) use 10mV / 10mA units on the wire,
        ; so the raw u16 from data is already in the right scale; applying
        ; a second x10 here would overflow the u16 and produce nonsense at
        ; the receiver.
        (bufset-u16 dataArray_0x7E5 0 raw-voltage)
        (bufset-u16 dataArray_0x7E5 2 raw-current)

        ; Immediately echo back ebs_get
        (send-ebs-get)
    })

    ; Control_Message_ID2 (0x4EF) - Report all ECU settings
    (if (= id 0x4EFu32) {
        (print "Full status request (0x4EF)")
        (send_stats)
        (sleep 0.01)
        (send-status4
            (if kers-enabled 1 0)
            1  ; gear_mode_enable is always true per KERS_GEAR_MODE_ENABLE
        )
        (sleep 0.01)
        (send-ebs-get)
        (sleep 0.01)
        (send-firmware-id)
        (sleep 0.01)
        (send-config-block)
        (sleep 0.01)
        (send-music-status)
        (send-engine-status)
    })

    ; Control_Message_ID1 (0x4EB) - Speed limit ratio
    (if (= id 0x4EBu32) {
        (var limit-pct (bufget-u8 data 0))
        (print (list "Speed limit:" limit-pct "%"))
        ; TODO: apply speed limit if needed
    })

    ; Control_Message_ID3 (0x4E1) - Gear set
    (if (= id 0x4E1u32) {
        (var gear (bufget-u8 data 0))
        (print (list "Gear set:" gear))
        ; TODO: apply gear mode if needed
    })

    ; Music control and song upload frames (0x4E8/0x4E9/0x4EC-0x4EE)
    (proc-music id data)

    (free data)
})

; Event handler thread
(defun event-handler ()
    (loopwhile t
        (recv
            ((event-can-sid (? id) . (? data)) (proc-sid id data))
            (_ nil)
        )
    )
)

(defun send_stats () {

    (bits-enc-int statusByte_1 0 1 2)
    (bits-enc-int statusByte_1 1 0 2)
    (bits-enc-int statusByte_1 2 0 2)
    (bits-enc-int statusByte_1 3 1 1)
    (bits-enc-int statusByte_1 4 0 1)
    (bits-enc-int statusByte_1 5 1 1)
    (bits-enc-int statusByte_1 6 1 1)
    (bits-enc-int statusByte_1 7 0 1)

    ; Voltage: (get-vin) returns V, ecu-service expects big-endian u16 in 10mV units
    (bufset-u16 dataArray_0x7E0 0 (to-i (* (get-vin) 100.0)))

    ; Current: (get-current-in) returns A, ecu-service expects big-endian i16 in 10mA units
    ; Negative values = regen (charging battery)
    (bufset-i16 dataArray_0x7E0 2 (to-i (* (get-current-in) 100.0)))

    (var speedhere (/ (* (get-speed) 3.6) speed-cal))

    (bufset-u16 dataArray_0x7E0 4 (to-i (/ (to-float (abs (get-rpm))) pole-pairs)))
    (bufset-u8 dataArray_0x7E0 6 speedhere)

    ; Flags: bit0 = throttle, bit1 = brake (regen active)
    (var flags 0)
    (if (> (get-current-in) 5.0)
        (setq flags (bitwise-or flags 0x01))
    )
    (if (< (get-current-in) -0.5)
        (setq flags (bitwise-or flags 0x02))
    )
    (bufset-u8 dataArray_0x7E0 7 flags)

    (bufset-u32 dataArray_0x7E2 0 (to-i (/ (to-float (sysinfo 'odometer)) 107.0)))

    (can-send-sid 0x7E0 dataArray_0x7E0)
    (sleep 0.01)
    (can-send-sid 0x7E2 dataArray_0x7E2)
    (sleep 0.01)
})

; Scale motor brake current with speed so KERS eases out at very low speed
; instead of riding the sensor handover. l-in-current-min alone does not do
; this: at low ERPM hardly any current makes it back to the battery, so the
; input limit never bites and the motor keeps getting full brake current right
; through the handover and down to standstill.
; Forward drive keeps the full configured motor-current envelope at any speed.
; Without drive, apply the standstill cutoff and KERS speed fade.
(defun kers-fade-target (erpm)
    (if (drive-requested)
        1.0
        (if (< erpm kers-hold-cutoff-erpm)
            0.0
            (if (not kers-enabled)
                1.0
                (if (< erpm kers-fade-end-erpm)
                    kers-floor
                    (if (> erpm kers-fade-start-erpm)
                        1.0
                        (+ kers-floor
                           (* (- 1.0 kers-floor)
                              (/ (- erpm kers-fade-end-erpm)
                                 (- kers-fade-start-erpm kers-fade-end-erpm))))
                    )
                )
            )
        )
    )
)

; Slew-limit toward the target so noisy ERPM cannot chatter the limit.
(defun update-kers-fade () {
    (var target (kers-fade-target (abs (to-float (get-rpm)))))
    (var d (- target kers-scale))
    (var new (if (drive-requested)
        target
        (if (> (abs d) kers-scale-slew)
            (+ kers-scale (if (> d 0.0) kers-scale-slew (- 0.0 kers-scale-slew)))
            target)))
    (if (> (abs (- new kers-scale)) 0.005) {
        (def kers-scale new)
        (conf-set 'l-current-min-scale new)
    })
})

; Battery-side regen voltage taper: full authority at or below the 0x4E2
; target, linear to zero across the taper band above it.
(defun kers-voltage-scale (vin-mv)
    (if (< vin-mv kers-voltage)
        1.0
        (if (> vin-mv (+ kers-voltage kers-voltage-taper))
            0.0
            (/ (- (+ kers-voltage kers-voltage-taper) vin-mv)
               kers-voltage-taper)
        )
    )
)

; Drive commands permit bounded regen regardless of speed or KERS enable.
; Brake commands use the 0x4E2 current while KERS is on, zero otherwise.
; Both envelopes respect the live pack-voltage taper.
(defun update-kers-current () {
    (var target (if kers-enabled
        (- 0.0 (/ (to-float kers-current) 1000.0))
        0.0))
    (if (and (drive-requested) (> target kers-drive-regen-floor))
        (setq target kers-drive-regen-floor))
    (setq target (* target (kers-voltage-scale (* (get-vin) 1000.0))))
    (if (> (abs (- target kers-amps-now)) 0.05) {
        (def kers-amps-now target)
        (conf-set 'l-in-current-min target)
    })
})

; Save the odometer once when the shutdown signal drops (power loss)
(defun check-shutdown () {
    (var v (get-adc 2))
    (if (and (< v shutdown-threshold) (not shutdown-triggered)) {
        (def shutdown-triggered true)
        (eeprom-store-f 0 (to-float (sysinfo 'odometer)))
    })
})

; ===== Boot =====
; image-save stores everything defined above in flash; from the next boot on the
; firmware re-creates that environment and calls main instead of parsing this
; file again. That is what lets the @const blocks above survive a power-on
; boot (re-parsing rewrote them and failed). Everything that touches hardware or
; must be fresh each boot (CAN buffers, config, native libs, threads) lives in
; main, since none of that is restored from the image.
(defun main () {
    (def dataArray_0x7E0 (array-create 8))
    (def dataArray_0x7E1 (array-create 6))
    (def dataArray_0x7E2 (array-create 8))
    (def dataArray_0x7E3 (array-create 8))
    (def dataArray_0x7E4 (array-create 8))
    (def dataArray_0x7E5 (array-create 4))
    (def dataArray_0x7E6 (array-create 5))   ; music: Music_Status
    (def dataArray_0x7E7 (array-create 4))   ; music: Music_Upload_Status
    (def dataArray_0x7E8 (array-create 8))
    (def dataArray_0x7E9 (array-create 2))
    (def dataArray_0x7EA (array-create 2))
    (def dataArray_0x7EB (array-create 1))
    (def dataArray_0x7EC (array-create 1))
    (def dataArray_0x7ED (array-create 4))
    (def dataArray_0x7EE (array-create 2))
    (def dataArray_0x7EF (array-create 2))

    (def song-buf-0 (array-create song-buf-max))  ; track slot 0: melody
    (def song-buf-1 (array-create song-buf-max))  ; track slot 1: bass
    (def channels (map (fn (x) (list x 0 0 0 0)) (range num-chan)))
    ; @private-warp-init

    ; The VESC keeps its config while powered, so after a lisp restart the limits
    ; would hold stale values (e.g. a regen limit armed by a handshake that the
    ; fresh script never saw). Force the safe boot state: no battery regen and no
    ; brake current until the loops take ownership and the vehicle asks for it.
    (conf-set 'l-in-current-min 0.0)
    (conf-set 'l-current-min-scale 0.0)

    ; Einmalig nach dem Einschalten Betriebsbereitschaft bestätigen
    (bufset-u8 dataArray_0x7E4 0 1)
    (bufset-u8 dataArray_0x7E4 1 100)
    (bufset-u8 dataArray_0x7E4 2 100)
    (bufset-u8 dataArray_0x7E4 3 100)
    (bufset-u8 dataArray_0x7E4 4 100)
    (bufset-u8 dataArray_0x7E4 5 100)
    (bufset-u8 dataArray_0x7E4 6 100)
    (bufset-u8 dataArray_0x7E4 7 100)
    (sleep 0.005)
    (can-send-sid 0x7E4 dataArray_0x7E4)

    (load-native-lib midi)
    (ext-midi-init 2) ; 2 simultaneous MIDI parsers = max 2 tracks per song
    (engine-init)
    (boost-start)

    ; Sustain envelope for the music engine
    (spawn 200 decay-loop)
    (spawn 300 engine-loop)

    ; Spawn the event handler thread and pass the ID it returns to C
    (event-register-handler (spawn 150 event-handler))

    ; Enable the CAN event for SID frames
    (event-enable 'event-can-sid)

    ; Restore odometer from EEPROM
    (def saved-odo (eeprom-read-f 0))
    (if saved-odo
        (set-odometer (to-i saved-odo))
    )

    ; Send initial status4 as not-kers (ebs_disabled)
    (send-status4 0 1)

    ; Broadcast identity and config at boot. Nothing requests 0x4EF on its own
    ; unless a fault was reported first.
    (sleep 0.01)
    (send-firmware-id)
    (sleep 0.01)
    (send-config-block)

    ; Song list lives in music-service (`music songs`), not here.
    (print "app.lisp loaded")

    ; Main loop: 5ms tick, shutdown check and KERS fade every tick, CAN stats every
    ; 40th tick (200ms), status2 every 200th (1s). A fault repeated faster than 1Hz
    ; keeps resetting the
    ; consumer's 0.5s request timer, so it never asks for the state that would
    ; clear the fault.
    (loopwhile t {
        (check-shutdown)
        (boost-tick)
        (update-kers-fade)
        (update-kers-current)
        (if (= (mod tick 40) 0) {
            (send_stats)
        })
        (if (= (mod tick 200) 0) {
            (send-status2)
        })
        (def tick (+ tick 1))
        (sleep 0.005)
    })
})

; LispBM loads some library helpers from source on first use. Done after an
; image boot, that load writes into the image's flash area at whatever spot is
; next, which a previous boot may already have filled differently ("already
; occupied") and kills the calling thread. Touch every such helper the code
; uses at runtime now, so they are baked into the image instead.
(list abs sin loopforeach looprange loopwhile)

(image-save)

; Start now as well; on later boots the firmware calls main itself.
(main)
