#lang racket
;; chord.rkt
;; Explorations in chord syntax

(require threading
         rakeda
         ;; define-with-spec
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
        'maj6    '(0 4 9) ; minor 1st inv
        'min6    '(0 3 8) ; major 1st inv
        'aug4    '(0 5 8) ; minor 2nd inv
        'x4+6    '(0 5 9) ; major 2nd inv

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
        'major+9 '(0 4 7 14) ; No 7th
        'minor+9 '(0 3 7 14) ; No 7th

        'maj+2   '(0 2 4 7)
        'min+2   '(0 2 3 7)
        'maj4+6  '(0 4 5 9)
        'min4+6  '(0 3 5 8)))

;; All chord names
(define chord-names (hash-keys chords))

;; Define the chord type
(struct chord (root name)
  #:transparent
  #:guard (λ (root name _)
            (unless (and (r:contains? root (flatten allNotes))
                         (r:contains? name chord-names))
              (error "Invalid fields: (chord <root> <name>)"))
            (values root name)))

;;-----------------------
(define (canonical ch)
  ;; Transpose a list down to start at 0
  (map+ (transpose (- (list-min ch))) ch))

;;-----------------------
(define (chord->num ch)
  ;; Convert a chord back to note numbers
  ;; chord->num :: Chord -> [Integer]
  (transpose (note->num (chord-root ch))
             (hash-ref chords (chord-name ch))))

(define (num->chord ch)
  ;; Do a reverse lookup of a chord name based on note numbers
  ;; If not recognised then just return the note names
  ;; lookup-chord :: [Integer] -> Chord
  (let* ([n (first ch)]
         [root (num->note* n)]
         [c (transpose (- n) ch)]
         [name (hash-lookup chords c)])
    (if (false? name)
        (num->note* ch)
        (chord root name))))

(define chord->notes (compose1 num->note* chord->num))

;;-----------------------
(define (inversions notes)
  ;; Show all inversions of a chord
  (r:iterate r:rotate-left (sub1 (length notes)) notes))

(define/curry (wrapc f ch)
  ;; Wrap a function that returns a list
  ;; e.g. (wrapc inversions (chord 'C 'major)) => '((chord 'E 'x4+6) (chord 'A 'major) (chord 'C# 'min6))
  (~>> ch
       chord->num
       f
       (r:map num->chord)))

;; The End
