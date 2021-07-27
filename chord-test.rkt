#lang racket

(require rackunit
         rackunit/text-ui
         "chord.rkt")

(define-test-suite chord-test

  (test-case "Chords"
    (check-equal? (c* 'G 'maj2) '(G A B D)))

  (test-case "Reverse chord lookup"
    (check-equal? (lookup-chord '(0 4 7)) '(C major))))

(run-tests chord-test)
;; The End
