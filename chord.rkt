#lang racket
;; chord.rkt
;; Explorations in chord syntax

(provide (all-defined-out))

(require threading
         rakeda
         ;; define-with-spec
         "core.rkt")

;;-----------------------
;; Utilities

;;-----------------------
(define chords
  (hash 'major   '(0 4 7)
        'minor   '(0 3 7)
        'dim     '(0 3 6)
        'aug     '(0 4 8)
        'x4+5    '(0 5 7)
        'maj4    '(0 4 5)
        'sus4    '(0 5 7)
        'min4    '(0 3 5)
        'maj6    '(0 4 9) ; minor 1st inv
        'min6    '(0 3 9)
        'aug4    '(0 5 8) ; minor 2nd inv
        'x4+6    '(0 5 9) ; major 2nd inv
        'b5      '(0 4 6)
        'min#5   '(0 3 8)

        ;; Sevenths
        'x7      '(0 4 7 10) ; dominant 7th
        'maj7    '(0 4 7 11)
        'aug7    '(0 4 8 10)
        'augmaj7 '(0 4 8 11)
        'min7    '(0 3 7 10)
        'minmaj7 '(0 3 7 11)
        'dim7    '(0 3 6 9)
        'dimmin7 '(0 3 6 10) ; half-diminished 7th
        'x7b5    '(0 4 6 10)

        ;; Ninths
        'x9      '(0 4 7 10 14) ; dominant 9th
        'maj9    '(0 4 7 11 14)
        'min9    '(0 3 7 10 14)
        'minmaj9 '(0 3 7 11 14)
        'add9    '(0 4 7 14) ; No 7th
        'minadd9 '(0 3 7 14) ; No 7th

        'maj+2   '(0 2 4 7)
        'min+2   '(0 2 3 7)
        'mixed3  '(0 3 4 7)
        'sus24   '(0 2 5 7)
        'maj4+6  '(0 4 5 9)
        'min4+6  '(0 3 5 8)
        'min+6   '(0 3 7 9)
        'minaug7 '(0 3 8 11)))

(define chord-names
  (hash-keys chords))

(define my-chords
  ;; The main chords I'm interested in
  '(major minor maj7 min7 maj9 min9))

(struct chord (root name)
  ;; Define the chord type
  ;; type Chord
  #:transparent
  #:guard (λ (root name _)
            (unless (and (r/in? root (flatten all-notes))
                         (r/in? name chord-names))
              (error "Invalid fields: (chord <root> <name>)"))
            (values root name)))

(define (random-chord)
  (let ([root (r/random-element note-names)]
        [ch (r/random-element chord-names)])
    (chord root ch)))

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
(define chord->notes* (compose1 num->note chord->num))
(define (list->chord lst) (chord (first lst) (last lst)))

;;-----------------------
(define (inversions notes)
  ;; Show all inversions of a chord
  (r/iterate r/rotate-left (sub1 (length notes)) notes))

(define/curry (wrapc f ch)
  ;; Wrap a function that returns a list
  ;; e.g. (wrapc inversions (chord 'C 'major)) => '((chord 'E 'x4+6) (chord 'A 'major) (chord 'C# 'min6))
  (~>> ch
       chord->num
       f
       (map++ num->chord)))

(define/curry (wrapc< f ch)
  ;; A note-sorted version of wrapc
  ;; e.g. (wrapc< P (chord 'C major)) => (chord 'C 'minor)
  (~>> ch
       chord->num
       f
       (map++ (r/sort <))
       (map++ num->chord)))

(define (contains-note? ch note)
  ;; Does a chord contain the given note?
  ;; common-note :: [Chord] -> Note -> Boolean
  (r/in? note (flatten (chord->notes* ch))))

(define (chord-contains note)
  ;; Which of my chords contain this note
  ;; chord-contains :: Note -> [Chord]
  (let ([ch (map list->chord
                 (cartesian-product note-names my-chords))])
    (filter (r/flip contains-note? note) ch)))

(define (random-related-chord ch #:change-root [chg-root #t])
  ;; Pick a random chord with at least one note in common
  ;; If `#:change-root` is true then it must have a different root note
  ;; related-chord :: Chord (-> Boolean) -> Chord
  (let ([note (r/random-element (chord->notes ch))])
    (~>> note
         chord-contains
         (r/filter (if chg-root
                       (λ (e) (r/not-eq? (chord-root e) (chord-root ch)))
                       true))
         r/random-element)))

(define (random-related-chords n)
  ;; Return n related chords, where each chord contains at least one note from the preceding chord.
  (r/iterate random-related-chord n (random-chord)))

(define (random-chords-with-note n note)
  ;; Generate n chords with a common note
  ;; random-chords-with-note :: Integer -> Note -> [Chord]
  (r/repeatedly n (λ () (r/random-element (chord-contains note)))))

;; The End
