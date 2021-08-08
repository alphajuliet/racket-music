#lang racket
;; sound.rkt
;; AndrewJ

(provide (all-defined-out))

(require rsound
         rsound/envelope)

;; Convert time in seconds to samples
(define (sec n)
  (* n FRAME-RATE))

;; ---------------------
;; Sound generators

(define (my-adsr att peak dec sus rel total)
  ;; Define an ADSR envelope
  (envelope-signal `((0 0.0)
                     (,att ,peak)
                     (,(+ att dec) ,sus)
                     (,(- total rel) ,sus)
                     (,total 0.0))))

(define (saw2 f)
  ;; Define a simple 2-osc detuned saw patch with a bit of vibrato
  ;; saw2 :: Real -> Signal
  (network ()
           [lfo1 <= sine-wave 5.0]
           [saw-1 <= sawtooth-wave (+ (* f 1.003)
                                      (* 0.6 lfo1))]
           [saw-2 <= sawtooth-wave (+ (* f 0.997)
                                      (* 0.6 lfo1))]
           [env <= (my-adsr (sec 0.2) 1.0 (sec 0.3) 0.8 (sec 0.3) (sec 1.0))]
           [mixer = (* 0.1 (+ saw-1 saw-2) env)]
           [out <= lpf/dynamic 0.4 mixer]))

(define (my-sine f)
  ;; Simple sine wave patch
  ;; sine :: Real -> Signal
  (network ()
           [osc <= sine-wave f]
           [out = (* 0.1 osc)]))

;; The End
