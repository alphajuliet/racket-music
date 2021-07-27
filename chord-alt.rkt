#lang racket
;; chord-alt.rkt
;; Explorations in chord syntax

(require rakeda
         "music.rkt")

(provide (all-defined-out))

;;-----------------------
(define chords
  ;; Define chords as a hash for reverse lookups
  (hash 'major   '(0 4 7)
        'minor   '(0 3 7)
        'x4+5    '(0 5 7)
        'x7      '(0 4 7 10)
        'maj7    '(0 4 7 11)
        'min7    '(0 3 7 10)
        'maj9    '(0 4 7 14)
        'min9    '(0 3 7 14)
        'maj7+9  '(0 4 7 11 14)
        'min7+9  '(0 3 7 10 14)
        'maj6    '(0 4 9)
        'min6    '(0 3 9)
        'aug     '(0 4 8)
        'maj2    '(0 2 4 7)
        'min2    '(0 2 3 7)
        'dim     '(0 3 6)
        'maj4    '(0 4 5)
        'min4    '(0 3 5)
        'maj4+6  '(0 4 5 9)
        'min4+6  '(0 3 5 9)))

;; Define the chord type
(struct chord (root name)
  #:transparent
  #:guard (λ (root name type)
            (unless (and (r:contains? root (flatten allNotes))
                         (r:contains? name (hash-keys chords)))
              (error "Invalid fields: (chord <root> <name>)"))
            (values root name)))

;;-----------------------
(define (chord->notes ch)
  ;; chord->notes :: Chord -> [Integer]
  (transpose (note->num (chord-root ch))
             (hash-ref chords (chord-name ch))))

(define (lookup-chord ch)
  ;; Do a reverse lookup of a chord name based on note numbers
  ;; lookup-chord :: [Integer] -> Chord
  (let* ([n (first ch)]
        [base (num->note* n)]
        [c (transpose (- n) ch)])
    (chord base (hash-lookup chords c))))

;; The End
