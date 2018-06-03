#lang racket
; sine-play.rkt

(require rsc3)

;; start udp SuperCollider server with: scsynth -u 57110

(with-sc3
    (lambda (fd)
      (send fd (g-new1 1 add-to-tail 0))))

;; play sin wave
(audition
 (out 0 (mul (sin-osc ar 440 0)
             0.1)))

;; reset
; (with-sc3 reset)