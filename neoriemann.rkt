#lang racket/base
;; neoriemann.rkt
;;
;; Define the PLR group of functions on triads from Neo-Riemannian Theory
;; (see Wikipedia entry at https://en.wikipedia.org/wiki/Neo-Riemannian_theory).
;; - P is an involution (i.e. a permutation with cycle length 2) between the major and
;;   minor triad of the same root: P(Cmaj) = Cmin
;; - R is an involution between relative major and minor triads: R(Cmaj) = Amin
;; - L doesn't have a recognised musical mapping: L(Cmaj) = Emin
;;----------------

(define (involute n1 n2 triad)
  ;; Generic function to apply involutions
  (map (invert (r:+ (r:nth n1 triad)
                    (r:nth n2 triad)))
       triad))

(define P (curry involute 0 2))
(define L (curry involute 1 2))
(define R (curry involute 0 1))
(define N (compose1 P L R))
(define S (compose1 R P L))
(define H (compose1 L P L))

;; The End