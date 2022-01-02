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
             (check-equal? (num->chord '(0 4 7)) (chord 'C 'major))
             (check-equal? (num->chord '(4 7 0)) (chord 'E 'min#5)))

  (test-case "Find notes in chords"
             (check-equal? (contains-note? (chord 'A# 'major) 'D) #t)
             (check-equal? (contains-note? (chord 'A# 'major) 'E) #f)
             (check-equal? (contains-note? (chord 'C 'minor) 'Eb) #t)
             (check-equal? (contains-note? (chord 'C 'minor) 'D#) #t)
             (check-equal? (length (chord-contains 'F)) 24)))

(run-tests chord-test)
;; The End
