#lang racket
;; core-test.rkt
;; Andrew 2018-06-24

(require rackunit
         rackunit/text-ui
         "core.rkt")

(define-test-suite music-tests

  ;; Utilities
  (test-case "Sanity"
    (check-equal? (+ 2 2) 4))

  (test-case "map+"
    (check-equal? (map+ add1 '(1 2 3)) '(2 3 4))
    (check-equal? (map+ add1 4) 5)
    (define inc (map+ add1))
    (check-equal? (inc 4) 5 ))

  (test-case "list-min"
    (check-equal? (list-min '(2 1 3)) 1))

  ;; Musical utilities
  (test-case "transpose"
    (define tr10 (transpose* 10))
    (check-equal? (transpose 4 6) 10)
    (check-equal? (tr10 5) 3)
    (check-equal? (transpose 8 '(0 4 7)) '(8 12 15))
    (check-equal? (transpose* 8 '(0 4 7)) '(8 0 3)))

  (test-case "num->note"
    (check-equal? (num->note '(1 3 4)) '((C# Db) (D# Eb) (E)))
    (check-equal? (num->note* 3) 'D#)
    (check-equal? (num->note* '(1 3 4)) '(C# D# E)))

  (test-case "note->num"
    (check-equal? (note->num 'G#) 8)
    (check-equal? (note->num 'Ab) 8))

  (test-case "collapse"
    (define x (num->note '(0 3 8)))
    (check-equal? (collapse 'C '(D# Eb)) 'D#)
    (check-equal? (collapse 'Db x) '(C Eb Ab))
    (check-equal? (collapse 'C# x) '(C D# G#)))

  (test-case "Wrapped functions"
    (check-equal? (wrap (transpose 2) '(C E G)) '(D F# A)))

  )

(run-tests music-tests)

; The End
