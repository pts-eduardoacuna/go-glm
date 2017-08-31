#lang racket/gui

(require math)
(require math/distributions)

(require "simplot.rkt")
(require "poten.rkt")
(require "thermo.rkt")

(define (plane-func w1 w2 b)
  (lambda (x1 x2)
    (+ b (* w1 x1) (* w2 x2))))

(define (plane-points w1 w2 b)
  (lambda (x1s x2s)
    (map (plane-func w1 w2 b) x1s x2s)))

(define plane-mapper (plane-points 1 -1/2 0.5))
(define samp (distribution-sample (uniform-dist 0 1)))
(define x1s (samp 20))
(define x2s (samp 20))
(define ys  (plane-mapper x1s x2s))
(define m   (length ys))


(define frame (new frame%
                   (label "plane model")
                   (min-width 800)
                   (min-height 400)))

(define layout (new horizontal-panel%
                    (parent frame)
                    (spacing 10)
                    (border 10)))

(define controls (new vertical-panel%
                      (parent layout)
                      (stretchable-width #f)))

(define plots (new vertical-panel%
                   (parent layout)
                   (spacing 5)))

(define monitor (new horizontal-panel%
                     (parent plots)
                     (stretchable-height #f)))

(define plot-group (new horizontal-panel%
                        (parent plots)
                        (spacing 5)))

(define w1 ((distribution-sample (uniform-dist -1 1))))
(define w2 ((distribution-sample (uniform-dist -1 1))))
(define b  ((distribution-sample (uniform-dist -1 1))))

(define (cost)
  (define ys* ((plane-points w1 w2 b) x1s x2s))
  (- (/ (apply + (map (lambda (y* y) (expt (- y* y) 2)) ys* ys)) m)))

(define J (cost))

(define thermo
  (new thermo%
       (parent monitor)
       (value J)
       (min-value -0.5)
       (max-value 0)))

(define (fx1 x1) (+ b (* w1 x1)))
(define (fx2 x2) (+ b (* w2 x2)))

(define (gx1 x1) (/ (+ b (* w1 x1)) (- w2)))

(define plot-x1-y
  (new simplot%
       (parent plot-group)
       (source-points (map cons x1s ys))
       (source-function fx1)))

(define plot-x2-y
  (new simplot%
       (parent plot-group)
       (source-points (map cons x2s ys))
       (source-function fx2)))

(define plot-x1-x2
  (new simplot%
       (parent plot-group)
       (source-points (map cons x1s x2s))
       (source-function gx1)))

(define control-w1
  (new poten%
       (parent controls)
       (label "w1")
       (notify-change (lambda (w1*)
                        (set! w1 w1*)
                        (set! J (cost))
                        (send plot-x1-y refresh)
                        (send plot-x1-x2 refresh)
                        (send thermo set-value! J)))
       (initial-value w1)))

(define control-w2
  (new poten%
       (parent controls)
       (label "w2")
       (notify-change (lambda (w2*)
                        (set! w2 w2*)
                        (set! J (cost))
                        (send plot-x2-y refresh)
                        (send plot-x1-x2 refresh)
                        (send thermo set-value! J)))
       (initial-value w2)))

(define control-b
  (new poten%
       (parent controls)
       (label "b")
       (notify-change (lambda (b*)
                        (set! b b*)
                        (set! J (cost))
                        (send plot-x1-y refresh)
                        (send plot-x2-y refresh)
                        (send plot-x1-x2 refresh)
                        (send thermo set-value! J)))
       (initial-value b)))

(send frame show #t)
