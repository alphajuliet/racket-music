#lang racket
;; Visualise chords on a circle

(require threading
         2htdp/image)

(provide (all-defined-out))

(define (pairs lst)
  (for/list ([i (range (sub1 (length lst)))])
    (list (list-ref lst i)
          (list-ref lst (add1 i)))))

;;----------------
;; Visualise chords

(struct pt (x y) #:transparent)

(define (pt+ p1 p2)
  ;; Add two points
  (pt (+ (pt-x p1) (pt-x p2))
      (+ (pt-y p1) (pt-y p2))))

(define (xy r n)
  ;; Translate r,n*pi/6 to x,y
  (let ([θ (/ pi 6)])
    (pt (* r (cos (* (- n 3) θ)))
        (* r (sin (* (- n 3) θ))))))

;;----------------
(define (draw-notes r base-shape)
  ;; Draw the points around the circumference
  (let ([note (circle 4 "solid" (color 100 100 255 255))])
    (foldl (λ (i result)
             (let ([org (xy r i)])
               (overlay/offset result (pt-x org) (pt-y org) note)))
           base-shape
           (range 0 12))))

(define (draw-chord ch r base-shape)
  ;; Draw a closed set of lines that corresponds to the chord
  (let ([pts (append ch (list (first ch)))]
        [origin (pt r r)])
    (foldl (λ (p result)
             (let ([a (pt+ origin (xy r (first p)))]
                   [b (pt+ origin (xy r (second p)))])
               (add-line result
                         (pt-x a) (pt-y a)
                         (pt-x b) (pt-y b)
                         (color 255 127 127 255))))
           base-shape
           (pairs pts))))

(define (view-chord ch)
  ;; Plot a chord on a circle
  (let* ([r 100]
         [base (circle r "outline" "white")])
    (~>> base
         (draw-notes r)
         (draw-chord ch r))))

;; The End
