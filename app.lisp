(def dataArray_0x7E0 (array-create 8))
(def dataArray_0x7E1 (array-create 8))
(def dataArray_0x7E2 (array-create 8))
(def dataArray_0x7E3 (array-create 8))
(def dataArray_0x7E4 (array-create 8))
(def dataArray_0x7E5 (array-create 4))
(def can-cnt 0)
(def statusByte_1 0x00)
(def tick 0)

; KERS state
(def kers-enabled false)
(def kers-voltage 0)
(def kers-current 0)

; Shutdown detection via PC4 (ADC channel 3)
; Steady-state ~2.69V, drops on power loss
(def shutdown-threshold 2.47)
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
            (conf-set 'l-current-min (* -1.0 kers-amps))
            (print (list "Regen enabled:" kers-amps "A"))
        } {
            (conf-set 'l-current-min 0.0)
            (print "Regen disabled")
        })

        ; Report status4 back so the service sees the correct state
        (send-status4 ebs-en gear-en)
    })

    ; ECU_ebs_set (0x4E2) - KERS voltage/current parameters
    ; Big-endian uint16 x2, scale=10 → real value in mV/mA
    (if (= id 0x4E2u32) {
        (var raw-voltage (bufget-u16 data 0))
        (var raw-current (bufget-u16 data 2))
        (def kers-voltage (* raw-voltage 10))
        (def kers-current (* raw-current 10))
        (print (list "KERS params V:" kers-voltage "mV, I:" kers-current "mA"))

        ; Store raw values to echo back on 0x7E5
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

    (var speedhere (/ (* (get-speed) 3.6) 1.03))

    (bufset-u16 dataArray_0x7E0 4 (to-i (/ (to-float (abs (get-rpm))) 24.0)))
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

; Main loop: 5ms tick, shutdown check every tick, CAN stats every 40th tick (200ms)
(loopwhile t {
    (check-shutdown)
    (if (= (mod tick 40) 0) {
        (send_stats)
    })
    (def tick (+ tick 1))
    (sleep 0.005)
})
