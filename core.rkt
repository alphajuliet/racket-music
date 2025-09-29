#lang racket
;; core.rkt
;; Define musical functions
;; AJ 2017-04-28

(provide (all-defined-out))

(require threading
         rakeda)

;; Contracts for better documentation and error checking
(define note-name? symbol?)
(define note-names? (listof note-name?))
(define note-number? integer?)
(define note-numbers? (listof note-number?))

;;-----------------------
;; Utility functions

(define/curry (map-notes f lst)
  ;; Apply function to notes, handling both single notes and lists of notes
  ;; More idiomatic version using match for clarity
  (match lst
    [(list (? list? _) ...) (map f lst)]  ; list of lists
    [_ (f lst)]))  ; single item

(define/curry (map++ f lst)
  ;; Generic map across a list or list of lists
  ;; map++ :: (a -> b) -> (a | [a]) -> (b | [b])
  (cond
    [(list? lst) (map f lst)]
    [else (f lst)]))

(define list-min
  ;; Minimum value of a list
  ;; list-min :: Sortable a ⇒ [a] → a
  ;; e.g. (list-min '(4 1 8 3)) => 1
  (curry apply min))

(define mod12
  ;; mod12 :: Integer -> Integer
  (r/flip modulo 12))

;;-----------------------
(define/curry (transpose n x)
  ;; Transpose a note or chord
  ;; transpose :: Integer -> Integer -> Integer
  (map++ (r/+ n) x))

(define/curry (transpose* n x)
  ;; Transpose a note or chord, modulo 12
  ;; e.g. (transpose* 14 '(0 3 7)) => '(2 5 9)
  ;; transpose* :: Integer -> Integer -> Integer
  (map++ (compose1 mod12 (r/+ n)) x))

(define/curry (invert n x)
  ;; invert :: Integer -> Integer -> Integer
  (map++ (r/- n) x))

(define/curry (invert* n x)
  ;; Invert modulo 12
  ;; invert* :: Integer -> Integer -> Integer
  (map++ (compose1 mod12 (r/- n)) x))

(define (inversions notes)
  ;; Show all inversions of note numbers
  ;; inversions :: [Integer] -> [[Integer]]
  ;; e.g. (inversions '(0 4 7)) => '((0 4 7) (4 7 0) (7 0 4))
  (if (list? notes)
      (let ([len (length notes)])
        (for/list ([i (in-range len)])
          (append (drop notes i) (take notes i))))
      notes))

;;-----------------------
;; Musical data structures and conversions

(define note-names
  ;; Note names, using my favourite mix of sharps and flats
  ;; define Note = Symbol
  ;; note-names :: [NoteName]
  '(C C# D Eb E F F# G Ab A Bb B))

(define all-notes
  ;; Comprehensive list of note names
  ;; all-notes :: [[NoteName]]
  '((C) (C# Db) (D) (D# Eb) (E) (F) (F# Gb) (G) (G# Ab) (A) (A# Bb) (B)))

(define (sharp-or-flat? note)
  ;; Is this note a sharp/flat?
  ;; sharp-or-flat? :: NoteName -> Boolean
  (define note-str (symbol->string note))
  (or (string-suffix? note-str "#")
      (string-suffix? note-str "b")))

;;-----------------------
(define (note->num notes)
  ;; Convert notes to numbers based on a C root
  ;; note->num :: NoteName → Integer || [NoteName] → [Integer]
  (define (note-index note)
    ;; Find the index of a note in all-notes, handling enharmonic equivalents
    (for/first ([note-group (in-list all-notes)]
                [index (in-naturals)]
                #:when (member note note-group))
      index))

  (cond
    [(list? notes) (map note-index notes)]
    [else (note-index notes)]))

(define collapse
  ;; Collapse notes based on the given reference note
  ;; C# -> D#, F#..., but Db -> Eb, Gb...
  ;; e.g. (collapse 'Eb '((C# Db) (F# Gb) (B))) => '(Db Gb B)
  ;; collapse :: NoteName -> [[NoteName]] -> [NoteName]
  (curry (λ (ref note)
           (let* ([ref-str (symbol->string ref)]
                  [use-flats? (and (= (string-length ref-str) 2)
                                   (string-suffix? ref-str "b"))])
             (if use-flats?
                 (map-notes last note)
                 (map-notes first note))))))

(define num->note
  ;; num->note :: [Integer] → [[NoteName]] || Integer → [NoteName]
  (map++ (compose1 (r/flip r/nth all-notes) mod12)))

(define num->note*
  ;; Collapsed version of num->note using sharps as default
  (compose (collapse 'C) num->note))
 
;;-----------------------
;; Map over note names
(define/curry (map-note f x)
  ;; Map a function on note numbers over note names
  ;; e.g. (map-note (transpose 2) '(C D E)) => '(D E F#)
  ;; e.g. (map-note inversions '(C E G)) => '((G C E) (C E G) (E G C))
  ;; map-note :: (Integer -> Integer) -> [NoteName] -> NoteName
  (~>> x
       (map++ note->num)
       f
       (map++ num->note*)))


;; The End
