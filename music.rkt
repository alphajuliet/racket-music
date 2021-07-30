#lang racket
;; music.rkt
;; Define musical functions
;; AJ 2017-04-28

(require threading
         rakeda)

(provide (all-defined-out))

;;-----------------------
;; Utility functions

(define/curry (map+ f x)
  ;; Generic map that is polymorphic across lists and values
  ;; map+ :: (a → b) → a → b
  ;; map+ :: (a → b) → [a] → [b]
  (if (list? x)
      (r/map f x)
      (f x)))

(define/curry (map++ f lst)
  ;; Generic map across a list or list of lists
  (if (and~> lst first list?)
      (map f lst)
      ;; else
      (f lst)))

(define list-min
  ;; Minimum value of a list
  ;; list-min :: Sortable a ⇒ [a] → a
  (r/argmin identity))

(define mod12
  (r/flip modulo 12))

(define (hash-lookup h value)
  ;; Return the first key that has the given value, otherwise #f
  ;; hash-lookup :: (Hash k v) -> v -> k
  (let ([lst (filter (λ (k) (equal? value (hash-ref h k)))
                     (hash-keys h))])
    (if (empty? lst)
        #f
        (first lst))))

;;-----------------------
(define/curry (transpose n x)
  ;; Transpose a note or chord
  (map+ (r/+ n) x))

(define/curry (transpose* n x)
  ;; Transpose a note or chord, modulo 12
  ;; e.g. (transpose* 14 '(0 3 7)) => '(2 5 9)
  (map+ (compose1 mod12 (r/+ n)) x))

(define/curry (invert n x)
  (map+ (r/- n) x))

(define/curry (invert* n x)
  ;; Invert modulo 12
  (map+ (compose1 mod12 (r/- n)) x))

;;-----------------------
;; Musical data structures and conversions

(define scale
  ;; scale :: [Integer]
  (range 0 12))

(define noteNames
  ;; Note names, using my favourite mix of sharps and flats
  ;; define NoteName = Symbol
  ;; noteNames :: [NoteNames]
  '(C C# D Eb E F F# G Ab A Bb B))

(define allNotes
  ;; Comprehensive list of note names
  '((C) (C# Db) (D) (D# Eb) (E) (F) (F# Gb) (G) (G# Ab) (A) (A# Bb) (B)))

;;-----------------------
(define (note->num n)
  ;; note->num :: NoteName → Integer || [NoteName] → [Integer]
  (map+ (λ (e) (r/index-where (curry true?)
                              (map (curry r/index-of e) allNotes)))
        n))

(define/curry (collapse ref note)
  ;; Collapse notes based on the given reference note
  ;; C# -> D#, F#..., but Db -> Eb, Gb...
  ;; e.g. (collapse 'Eb '((C# Db) (F# Gb) (B))) => '(Db Gb B)
  (let ([ref-note (symbol->string ref)])
    (if (and ((string-length ref-note) . = . 2)
             (eq? #\b (string-ref ref-note 1)))
        (map++ last note)
        ;; else
        (map++ first note))))

(define num->note
  ;; num->note :: [Integer] → [[NoteName]] || Integer → [NoteName]
  (map+ (compose1 (r/flip r/nth allNotes) mod12)))

(define num->note*
  ;; Collapse the note options
  (compose (collapse 'C) num->note))
 
;;-----------------------
;; Define better wrappers

(define/curry (wrap f x)
  ;; Wrap note->num and num->note
  ;; e.g. (wrap (transpose 2) '(C D E)) => '(D E F#)
  ;;      (wrap maj2 'G) => '(G A B D)
  (~>> x
       note->num
       f
       num->note*))

(define/curry (wrapl f x)
  ;; Wrap a function that returns a list
  ;; e.g. (wrapl inversions '(C E G)) => '((G C E) (C E G) (E G C))
  (~>> x
       note->num
       f
       (r/map num->note*)))

;;----------------
(displayln ":: Loaded musical definitions.")

;; The End
