#lang racket

(require rackunit
         rackunit/text-ui
         "scale.rkt")

(define-test-suite scale-test

  (test-case "Scales"
             (check-equal? (scale->num (scale 'G 'major-pentatonic)) '(7 9 11 14 16))
             (check-equal? (scale->notes* (scale 'G 'lydian)) '(G A B C# D E F#))))

(run-tests scale-test)
;; The End
