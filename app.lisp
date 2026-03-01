(def dataArray_0x7E0 (array-create 8))
(def dataArray_0x7E1 (array-create 8))
(def dataArray_0x7E2 (array-create 8))
(def dataArray_0x7E3 (array-create 8))
(def dataArray_0x7E4 (array-create 8))
(def can-cnt 0)
(def statusByte_1 0x00)
(def tick 0)

; Shutdown detection via PC4 (ADC channel 3)
; Steady-state ~2.69V, drops on power loss
(def shutdown-threshold 2.55)
(def shutdown-triggered false)

; einmalig nach dem einschalten Betriebsbereitschaft bestätigen
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


; Handler for SID CAN-frames
(defun proc-sid (id data) {

    (if (= id 0x4E0u32)
        (print "0x4E0u32 CAN Frame")
    )
    (if (= id 0x4E1u32)
        (print "0x4E2u32 CAN Frame2")
    )
    (if (= id 0x4E2u32)
        (print "0x4E4u32 CAN Frame3")
    )

    (if (= id 0x4E4u32) {
        (print "0x4E4u32 CAN Frame_4")
        (can-send-sid 0x7E3 dataArray_0x7E3)
        (sleep 0.050)
        (can-send-sid 0x7E4 dataArray_0x7E4)
        (sleep 0.050)
        (can-send-sid 0x7E5 (list 0xAA 0x11 0x15))
        (sleep 0.050)
    })

    ; usage from increasing more than needed.
    (free data)
})

; This function waits for events from the C code and calls the
; handlers for them when the events arrive.
(defun event-handler ()
    (loopwhile t
        (recv
            ((event-can-sid (? id) . (? data)) (proc-sid id data))
            (_ nil)
        )))


(defun send_stats () {

    (bits-enc-int statusByte_1 0 1 2)
    (bits-enc-int statusByte_1 1 0 2)
    (bits-enc-int statusByte_1 2 0 2)
    (bits-enc-int statusByte_1 3 1 1)
    (bits-enc-int statusByte_1 4 0 1)
    (bits-enc-int statusByte_1 5 1 1)
    (bits-enc-int statusByte_1 6 1 1)
    (bits-enc-int statusByte_1 7 0 1)

    (def speedhere (/ (* (get-speed) 3.6) 1.03))
    ; (bufset-u16 dataArray_0x7E0 0 (/ (get-vin) 0.01))
    ; (bufset-i16 dataArray_0x7E0 2 (/ (get-current-in) 0.01))
    ;(bufset-u16 dataArray_0x7E0 4 speedhere)
    (bufset-u16 dataArray_0x7E0 4 (to-i (/ (to-float (abs (get-rpm))) 24.0)))
    (bufset-u8 dataArray_0x7E0 6 speedhere)

    (bufset-u8 dataArray_0x7E0 7 0)
    (if (< (get-current-in) -1)
        ;(bufset-u8 dataArray_0x7E0 7 0x02)
    )
    (if (> (get-current-in) 5)
        (bufset-u8 dataArray_0x7E0 7 0x01)
    )
    ;(bufset-i8 dataArray_0x7E1 0 (get-temp-fet))

    (bufset-u32 dataArray_0x7E2 0 (to-i (/ (to-float (sysinfo 'odometer)) 107.0)))

    ;(bufset-u8 dataArray_0x7E3 0 0x96)

    (can-send-sid 0x7E0 dataArray_0x7E0)
    (sleep 0.01)
    ; (can-send-sid 0x7E1 dataArray_0x7E1)
    ; (sleep 0.001)
    (can-send-sid 0x7E2 dataArray_0x7E2)
    (sleep 0.01)
    ; (can-send-sid 0x7E3 dataArray_0x7E3)
    ; (sleep 0.001)
})

; Check PC4 voltage and save odometer/runtime on shutdown
(defun check-shutdown () {
    (let ((v (get-adc 3)))
        (if (and (< v shutdown-threshold) (not shutdown-triggered)) {
            (def shutdown-triggered true)
            (eeprom-store-f 0 (to-float (sysinfo 'odometer)))
        })
    )
})


; Spawn the event handler thread and pass the ID it returns to C
(event-register-handler (spawn 150 event-handler))

; Enable the CAN event for SID frames
(event-enable 'event-can-sid)

(let ((saved-odo (eeprom-read-f 0)))
    (if saved-odo
        (set-odometer (to-i saved-odo))
    )
)

; Main loop: 5ms tick, shutdown check every tick, CAN stats every 40th tick (200ms)
(loopwhile t {
    (check-shutdown)
    (if (= (mod tick 40) 0) {
        (send_stats)
    })
    (def tick (+ tick 1))
    (sleep 0.005)
})
