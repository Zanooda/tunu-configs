(def dataArray_0x7E0 (array-create 8))
(def dataArray_0x7E1 (array-create 6))
(def dataArray_0x7E2 (array-create 8))
(def dataArray_0x7E3 (array-create 8))
(def dataArray_0x7E4 (array-create 8))
(def dataArray_0x7E5 (array-create 4))
(def dataArray_0x7E8 (array-create 8))
(def dataArray_0x7E9 (array-create 2))
(def dataArray_0x7EA (array-create 2))
(def dataArray_0x7EB (array-create 1))
(def dataArray_0x7EC (array-create 1))
(def dataArray_0x7EE (array-create 2))
(def dataArray_0x7EF (array-create 2))
(def can-cnt 0)
(def statusByte_1 0x00)
(def tick 0)

; 0x7E8 bytes 4-7: rated power (BCD kW), max speed (BCD km/h), base SW version
; (BCD nibbles), app revision (plain byte).
(def motor-power-kw 0x03)
(def max-speed-kmh 0x45)
(def sw-base-version 0x10)  ; "1.0"
(def sw-app-version 1)

; Values mirrored from the mcconf XMLs (identical in both variants). This
; firmware's conf-get does not recognize these parameter names (type_error on
; script load), so they cannot be read from the live config.
(def max-vin 65.0)
(def min-vin 20.0)
(def wheel-diameter 0.416)
(def current-max 210.0)
(def pole-pairs 24.0)

; Empirical speed/odometer calibration, matches CalibrationFactor in ecu-service.
(def speed-cal 1.03)

; KERS state
(def kers-enabled false)
(def kers-voltage 0)
(def kers-current 0)

; Low-speed KERS fade, in ERPM so it tracks the FOC sensor handover directly.
; Coming down, the observer hands back to the halls between foc_sl_erpm (2000)
; and foc_sl_erpm_start (1800). Braking through that blend at full current is
; what rattles, so the fade bottoms out at a floor just above the blend and the
; floor carries through it, keeping the lever authoritative at crawl. The fade
; band sits at very low speed: full regen down to ~8.5 km/h, floor below that.
; Below the standstill cutoff the halls only give a coarse sector angle, and
; brake current there can turn into a slight forward pull, so the scale drops
; to zero unconditionally — KERS on or off. (The BOSCH 4K-45 ECU does the
; equivalent with its motor-power deadband: regen disengages near standstill.)
; With 48 poles and the 0.416 m wheel: 2200 erpm ~ 7.2 km/h, 2600 ~ 8.5, 300 ~ 1.
(def kers-fade-start-erpm 2600.0)
(def kers-fade-end-erpm 2200.0)
(def kers-floor 0.25)
(def kers-hold-cutoff-erpm 300.0)
; Max change in scale per 5 ms tick: engage ramps in fast but not as a step
; (~170 ms full sweep), and a noisy get-rpm in the fade band cannot chatter the
; limit. The lever command itself is additionally ramped by the ADC app
; (ramp_time_pos 0.3 s) and battery regen stays capped at the 0x4E2 current
; limit (10 A default), so the pack never sees a current step.
(def kers-scale-slew 0.03)
(def kers-scale 1.0)

; Shutdown detection via PC4 (ADC channel 3)
; Steady-state ~2.69V, drops on power loss
(def shutdown-threshold 2.48)
(def shutdown-triggered false)

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

; Build and send ECU_status4 (0x7E3) reflecting current KERS state
(defun send-status4 (ebs-en gear-en) {
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

; Send ECU_ebs_get (0x7E5) echoing back current KERS parameters
(defun send-ebs-get () {
    (can-send-sid 0x7E5 dataArray_0x7E5)
})

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
    ; No FET temperature sensor on this controller: get-temp-fet reads a
    ; floating ADC and climbs into triple digits. Pin a plausible ambient
    ; 25 °C instead of 0, which still reads as freezing to consumers.
    (bufset-i8 dataArray_0x7E1 0 25)
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

; Send the ECU configuration block (0x7E9-0x7EF) from the mirrored mcconf
; constants above. 10mV / 10mA units on the wire.
(defun send-config-block () {
    (bufset-u16 dataArray_0x7E9 0 (to-i (* max-vin 100)))
    (can-send-sid 0x7E9 dataArray_0x7E9)
    (sleep 0.005)

    (bufset-u16 dataArray_0x7EA 0 (to-i (* min-vin 100)))
    (can-send-sid 0x7EA dataArray_0x7EA)
    (sleep 0.005)

    ; No VESC equivalent, report 100% so nothing infers a derate.
    (bufset-u8 dataArray_0x7EB 0 100)
    (can-send-sid 0x7EB dataArray_0x7EB)
    (sleep 0.005)

    ; Rolling circumference in cm, with the same 1.03 calibration send_stats
    ; applies to the reported km/h.
    (bufset-u8 dataArray_0x7EC 0
        (to-i (/ (* wheel-diameter 3.14159 100) speed-cal)))
    (can-send-sid 0x7EC dataArray_0x7EC)
    (sleep 0.005)

    (bufset-u16 dataArray_0x7EE 0 (to-i (* current-max 100)))
    (can-send-sid 0x7EE dataArray_0x7EE)
    (sleep 0.005)

    ; No separate startup current on VESC, mirror the peak limit.
    (bufset-u16 dataArray_0x7EF 0 (to-i (* current-max 100)))
    (can-send-sid 0x7EF dataArray_0x7EF)
    (sleep 0.005)
})

; Handler for SID CAN-frames
(defun proc-sid (id data) {

    ; Control_Message_ID4 (0x4E0) - KERS/boost/gear enable
    ; Byte 0: bit0=gear_mode_enable, bit1=boost_mode_enable, bit2=ebs_enable
    (if (= id 0x4E0u32) {
        (var ctrl-byte (bufget-u8 data 0))
        (var gear-en  (bitwise-and ctrl-byte 0x01))
        (var boost-en (shr (bitwise-and ctrl-byte 0x02) 1))
        (var ebs-en   (shr (bitwise-and ctrl-byte 0x04) 2))

        (def kers-enabled (= ebs-en 1))
        (print (list "KERS:" kers-enabled "gear:" gear-en "boost:" boost-en))

        ; Toggle regen current limit on VESC
        ; l-current-min is negative (regen direction), 0.0 = no regen allowed
        (if kers-enabled {
            (var kers-amps (/ (to-float kers-current) 1000.0))
            (conf-set 'l-in-current-min (* -1.0 kers-amps))
            (print (list "Regen enabled:" kers-amps "A"))
        } {
            (conf-set 'l-in-current-min 0.0)
            (print "Regen disabled")
        })

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
; Target scale for the current ERPM: zero below the standstill cutoff (always —
; this is what kills the forward pull at crawl); with KERS off, full above it
; (the lever brake is dissipative then, l-in-current-min is 0); with KERS on,
; the floor below the fade band, a floor-to-full ramp across the band, and full
; above it.
(defun kers-fade-target (erpm)
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

; Slew-limit toward the target so noisy ERPM cannot chatter the limit.
(defun update-kers-fade () {
    (var target (kers-fade-target (abs (to-float (get-rpm)))))
    (var d (- target kers-scale))
    (var new (if (> (abs d) kers-scale-slew)
        (+ kers-scale (if (> d 0.0) kers-scale-slew (- 0.0 kers-scale-slew)))
        target))
    (if (> (abs (- new kers-scale)) 0.005) {
        (def kers-scale new)
        (conf-set 'l-current-min-scale new)
    })
})

; Check PC4 voltage and save odometer/runtime on shutdown
(defun check-shutdown () {
    (var v (get-adc 3))
    (if (and (< v shutdown-threshold) (not shutdown-triggered)) {
        (def shutdown-triggered true)
        (eeprom-store-f 0 (to-float (sysinfo 'odometer)))
    })
})

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

; Main loop: 5ms tick, shutdown check and KERS fade every tick, CAN stats every
; 40th tick (200ms), diagnostics every 200th (1s). A fault repeated faster than
; 1Hz keeps resetting the consumer's 0.5s request timer, so it never asks for
; the state that would clear the fault.
(loopwhile t {
    (check-shutdown)
    (update-kers-fade)
    (if (= (mod tick 40) 0) {
        (send_stats)
    })
    (if (= (mod tick 200) 0) {
        (send-status2)
    })
    (def tick (+ tick 1))
    (sleep 0.005)
})

