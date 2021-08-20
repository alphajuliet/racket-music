#lang racket
;; visual.rkt
;; Visualise chords on a circle

;;----------------
;; Imports and exports

(provide view-chord
         view-piano-chord)

(require threading
         rakeda
         "chord.rkt"
         2htdp/image
         (prefix-in p: pict))

;;----------------
;; Utilities

(define (pairs lst)
  (for/list ([i (range (sub1 (length lst)))])
    (list (list-ref lst i)
          (list-ref lst (add1 i)))))

;;----------------
;; Visualise chords

;; 2D point
(struct pt (x y) #:transparent)

(define (pt+ p1 p2)
  ;; Add two points
  (pt (+ (pt-x p1) (pt-x p2))
      (+ (pt-y p1) (pt-y p2))))

(define (xy r n)
  ;; Translate (r, n*pi/6) to (x, y)
  (let ([θ (/ pi 6)])
    (pt (* r (cos (* (- n 3) θ)))
        (* r (sin (* (- n 3) θ))))))

;;----------------
(define (draw-notes r base-shape)
  ;; Draw the points around the circumference
  (let ([note (circle 4 "solid" "dodgerblue")])
    (foldl (λ (i result)
             (let ([loc (xy r i)])
               (overlay/offset result (pt-x loc) (pt-y loc) note)))
           base-shape
           (range 0 12))))

(define (draw-first-note n r base-shape)
  ;; Highlight the first note in the chord
  (let ([loc (xy r n)])
    (overlay/offset base-shape
                    (pt-x loc) (pt-y loc)
                    (circle 6 "solid" "aqua"))))

;;----------------
(define (draw-chord ch r base-shape)
  ;; Draw a series of lines that corresponds to the chord
  (let ([origin (pt r r)])
    (foldl (λ (p result)
             (let ([a (pt+ origin (xy r (first p)))]
                   [b (pt+ origin (xy r (second p)))])
               (add-line result
                         (pt-x a) (pt-y a)
                         (pt-x b) (pt-y b)
                         (color 255 127 127 255))))
           base-shape
           (pairs ch))))

;;----------------
(define (view-chord ch)
  ;; Plot a chord on a circle
  (let* ([radius 100]
         [note-nums (chord->num ch)]
         [base (circle radius "outline" "white")])
    (~>> base
         (draw-first-note (first note-nums) radius)
         (draw-chord note-nums radius)
         (draw-notes radius))))

;;----------------
;; Draw a piano keyboard

(define (draw-octave)
  (let ([rw (p:filled-rectangle 20 100 #:color "white")]
        [rb (p:filled-rectangle 12 60 #:color "black")])
    (p:pin-over
     (p:ht-append rw rw rw rw rw rw rw)
     14 0
     (p:pin-over
      (p:ht-append 8 rb rb)
      60 0
      (p:ht-append 8 rb rb rb)))))

(define (draw-piano (octaves 2))
  (apply p:hc-append (build-list octaves (λ (_) (draw-octave)))))

(define note-coords
  ;; Coords of each note on the keyboard
  (list (pt 5 80)   ; C
        (pt 15 40)  ; C#/Db
        (pt 25 80)  ; D
        (pt 35 40)  ; D#/Eb
        (pt 45 80)  ; E
        (pt 65 80)  ; F
        (pt 75 40)  ; F#/Gb
        (pt 85 80)  ; G
        (pt 95 40)  ; G#/Ab
        (pt 105 80) ; A
        (pt 115 40) ; A#/Bb
        (pt 125 80))) ; B

(define (piano-note num (base (draw-piano)))
  ;; Draw a dot on the note number on the keyboard
  (let* ([octave (quotient num 12)]
         [note-num (remainder num 12)]
         [xy (list-ref note-coords note-num)])
    (p:pin-over base
                (+ (pt-x xy) (* 140 octave)) (pt-y xy)
                (p:filled-ellipse 10 10 #:color "cyan"))))

(define (piano-notes note-list)
  (foldl (λ (n acc) (piano-note n acc))
         (draw-piano)
         note-list))

(define (view-piano-chord ch)
  (piano-notes (chord->num ch)))

;; The End
