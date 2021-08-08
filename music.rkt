#lang racket
;; music.rkt
;; Racket music top-level
;; AndrewJ 2021-08-07

(provide (all-defined-out))

(require threading
         rakeda
         "core.rkt"
         "chord.rkt"
         "neoriemann.rkt"
         "audio.rkt"
         "sound.rkt"
         "visual.rkt")

(define (ex1)
  (for ([n (in-range 4)]
        [ch (random-chords-with-note 4 'F)])
    (play-chord saw2 ch (beats (* 4 n)) (beats 3.5))))

;; The End
