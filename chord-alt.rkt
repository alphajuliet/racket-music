#lang racket
;; chord-alt.rkt
;; Explorations in chord syntax

(require threading
         rakeda
         "music.rkt")

(provide (all-defined-out))

;;-----------------------
(define chords
  (hash 'major   '(0 4 7)
        'minor   '(0 3 7)
        'dim     '(0 3 6)
        'aug     '(0 4 8)
        'x4+5    '(0 5 7)
        'maj4    '(0 4 5)
        'min4    '(0 3 5)
        'maj6    '(0 4 9)
        'min6    '(0 3 8)
        'x4+6    '(0 5 9)
        'aug4    '(0 5 8)

        ;; Sevenths
        'x7      '(0 4 7 10) ; dominant 7th
        'maj7    '(0 4 7 11)
        'aug7    '(0 4 8 10)
        'augmaj7 '(0 4 8 11)
        'min7    '(0 3 7 10)
        'minmaj7 '(0 3 7 11)
        'dim7    '(0 3 6 9)
        'dimmin7 '(0 3 6 10) ; half-diminished 7th

        ;; Ninths
        'x9      '(0 4 7 10 14) ; dominant 9th
        'maj9    '(0 4 7 11 14)
        'min9    '(0 3 7 10 14)
        'minmaj9 '(0 3 7 11 14)

        'maj+2   '(0 2 4 7)
        'min+2   '(0 2 3 7)
        'maj4+6  '(0 4 5 9)
        'min4+6  '(0 3 5 8)

        'maj7+9  '(0 4 7 11 14)
        'min7+9  '(0 3 7 10 14)))

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

(define (notes->chord ch)
  ;; Do a reverse lookup of a chord name based on note numbers
  ;; lookup-chord :: [Integer] -> Chord
  (let* ([n (first ch)]
         [root (num->note* n)]
         [c (transpose (- n) ch)]
         [name (hash-lookup chords c)])
    (if (false? name)
        (num->note* ch)
        (chord root name))))

(define (inversions notes)
  ;; Show all inversions of a chord
  (r:nest-list r:rotate-left (length notes) notes))

(define (wrapc f ch)
  ;; Wrap a function that returns a list
  ;; e.g. (wrapc inversions (chord 'C 'major)) => '((G C E) (C E G) (E G C))
  (~>> ch
       chord->notes
       f
       (r:map notes->chord)))
;; The End
