#lang racket

(require rackunit
         rackunit/text-ui
         "chord.rkt")

(define-test-suite chord-test

  (test-case "Chords"
             (check-equal? (chord->num (chord 'G 'maj+2)) '(7 9 11 14))
             (check-equal? (chord->notes (chord 'G 'maj+2)) '(G A B D)))

  (test-case "Canonical"
             (check-equal? (canonical '(6 2 8)) '(4 0 6)))

  (test-case "Reverse chord lookup"
             (check-equal? (num->chord '(0 4 7)) (chord 'C 'major))))

(run-tests chord-test)
;; The End
