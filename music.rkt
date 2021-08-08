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

(define (example-1 (note 'F))
  ;; Play four related chords with a common note.
  (for ([n (in-range 4)]
        [ch (random-chords-with-note 4 note)])
    (play-chord saw2 ch (beats (* 4 n)) (beats 3.9))))

;; The End
