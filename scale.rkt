#lang racket
;; scale.rkt
;; Explorations in scales

(provide (all-defined-out))

(require threading
         rakeda
         ;; define-with-spec
         "core.rkt")

;;-----------------------
;; Definitions

(define scales
  (hash
   'major               '(0 2 4 5 7 9 11)
   'dorian              '(0 2 3 5 7 9 10)
   'phrygian            '(0 1 3 5 7 8 10)
   'lydian              '(0 2 4 6 7 9 11)
   'mixolydian          '(0 2 4 5 7 9 10)
   'aeolian             '(0 2 3 5 7 8 10)
   'locrian             '(0 1 3 5 6 8 10)
   'minor               '(0 2 3 5 7 8 11)
   'harmonic-major      '(0 2 4 5 7 8 11)
   'blues               '(0 3 5 6 7 10)
   'chromatic           '(0 1 2 3 4 5 6 7 8 9 10 11)
   'major-pentatonic    '(0 2 4 7 9)
   'minor-pentatonic    '(0 3 4 7 10)
   'neapolitan-major    '(0 1 3 5 7 9 11)
   'neapolitan-minor    '(0 1 3 5 7 8 11)
   'tritone             '(0 1 4 6 7 10)))

(define scale-names
  (hash-keys scales))

(struct scale (root name)
  ;; Define the scale type
  ;; type Scale
  #:transparent
  #:guard (λ (root name _)
            (unless (and (r/in? root (flatten all-notes))
                         (r/in? name scale-names))
              (error "Invalid fields: (scale <root> <name>)"))
            (values root name)))

(define (scale->num sc)
  ;; Convert a scale back to note numbers
  ;; scale->num :: Scale -> [Integer]
  (transpose (note->num (scale-root sc))
             (hash-ref scales (scale-name sc))))

(define scale->notes (compose1 num->note scale->num))
(define scale->notes* (compose1 num->note* scale->num))

;; The End
