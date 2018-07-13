#lang racket
; music-test.rkt
; Andrew 2018-06-24

(require rackunit
         rackunit/text-ui
         "music.rkt")

(define-test-suite music-tests

  ; Utilities
  (test-case "Sanity"
             (check-equal? (+ 2 2) 4))
  (test-case "map+"
             (check-equal? (map+ add1 '(1 2 3)) '(2 3 4))
             (check-equal? (map+ add1 4) 5)
             (define f (map+ add1))
             (check-equal? (f 4) 5 ))
  (test-case "list-min"
             (check-equal? (list-min '(2 1 3)) 1))

  ; Musical utilities
  (test-case "transpose"
             (check-equal? (transpose 4 6) 10)
             (check-equal? (transpose 10 5) 3)
             (check-equal? (transpose 8 '(0 4 7)) '(8 0 3)))
  (test-case "canonical"
             (check-equal? (canonical '(6 2 8)) '(0 4 6)))
  (test-case "num->note"
             (check-equal? (num->note 2) 'D)
             (check-equal? (num->note '(0 2 4)) '(C D E)))
  (test-case "note->num"
             (check-equal? (note->num 'E) 4)
             (check-equal? (note->num '(C D E)) '(0 2 4)))
  (test-case "sorted-num->note"
             (check-equal? (sorted-num->note '(4 2 0)) '(C D E)))

  ; Chords
  (test-case "sc"
             (check-equal? (sc 'F maj2) '(F G A C)))
  (test-case "Wrapped functions"
             (check-equal? (tr* 4 (sc 'F# maj7)) '(Bb D F A)))
  
  )

(run-tests music-tests)

; The End