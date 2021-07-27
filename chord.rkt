#lang racket
;; chord.rkt

(require rakeda
         "music.rkt")

(provide (all-defined-out))

;;-----------------------
;; Define a chord as a list of intervals from the base note.

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

(define (c note ch)
  ;; Return the note numbers for the given chord
  (let ([n (note->num note)])
    (r:map (transpose n) (hash-ref chords ch))))

(define c*
  ;; Return the chord as note names
  (compose num->note* c))

(define main-chords
  ;; Just the important ones
  ;; main-chords :: [Chord]
  (list 'major 'minor 'x7 'maj7 'min7))

(define all-chords
  ;; all-chords :: [Chord]
  (hash-keys chords))

(define/c (apply-chord chs note)
  ;; Show chords for a given base note.
  (map+ (compose num->note* (c* note)) chs))

(define (match-chord lst)
  ;; Match note numbers to one or more chord names
  ;; This will do a match across all the given chords
  ;; @@TODO
  (let ([x (map canonical (permutations lst))])
    #f))

(define (lookup-chord ch)
  ;; Do a reverse lookup of a chord name based on note numbers
  ;; lookup-chord :: [Integer] -> (NoteName Chord)
  (let* ([n (first ch)]
        [base (num->note* n)]
        [c (transpose (- n) ch)])
    (list base (hash-lookup chords c))))

(define (inversions ch)
  ;; Show all inversions of a chord
  (r:nest-list r:rotate-left (length ch) ch))

;;----------------
;; Example usage of the above functions

;; (wrap (compose (transpose 2) min7) 'G)
;; (wrapl (compose inversions min6) 'E)
;; (show-main-chords 'D)
;;
;; The End
