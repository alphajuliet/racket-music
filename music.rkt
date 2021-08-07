#lang racket
;; music.rkt
;; Racket music top-level
;; AndrewJ 2021-08-07

(provide (all-defined-out))

(require "core.rkt"
         "chord.rkt"
         "neoriemann.rkt"
         "audio.rkt"
         "visual.rkt")

(define (ex1)
  (let ([chords (random-chords-with-note 4 'F)])
    (map (λ (ch) (play-chord++ saw2 ch)) chords)))

;; The End
