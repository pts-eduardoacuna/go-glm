#lang racket/gui

(require plot)

(require math)
(require math/distributions)

(require "simplot.rkt")
(require "poten.rkt")

(define (plane-func w1 b)
  (lambda (x1)
    (+ b (* w1 x1))))

(define (plane-points w1 b)
  (lambda (x1s)
    (map (plane-func w1 b) x1s)))

(define plane-mapper (plane-points 1 0.5))
(define samp (distribution-sample (uniform-dist 0 1)))
(define x1s (samp 20))
(define ys  (plane-mapper x1s))


(define frame (new frame%
                   (label "line model")
                   (min-width 800)
                   (min-height 400)))

(define layout (new horizontal-panel%
                    (parent frame)
                    (spacing 10)
                    (border 10)))

(define controls (new vertical-panel%
                      (parent layout)
                      (stretchable-width #f)))

(define plots (new horizontal-panel%
                   (parent layout)
                   (border 5)
                   (spacing 5)))

(define w1 1.0)
(define b  0.0)

(define (fx1 x1) (+ b (* w1 x1)))

(define plot-x1-y
  (new simplot%
       (parent plots)
       (source-points (map cons x1s ys))
       (source-function fx1)))

(define control-w1
  (new poten%
       (parent controls)
       (label "w1")
       (notify-change (lambda (w1*)
                        (set! w1 w1*)
                        (send plot-x1-y refresh)))
       (initial-value w1)))

(define control-b
  (new poten%
       (parent controls)
       (label "b")
       (notify-change (lambda (b*)
                        (set! b b*)
                        (send plot-x1-y refresh)))
       (initial-value b)))

(define x1-min (apply min x1s))
(define x1-max (apply max x1s))
(define y-min (apply min ys))
(define y-max (apply max ys))

(plot-new-window? #t)
(plot (points (map list x1s ys)
              #:sym 'fullcircle
              #:x-min (if (< x1-min 0) x1-min 0)
              #:x-max (if (> x1-max 1) x1-max 1)
              #:y-min (if (< y-min 0) y-min 0)
              #:y-max (if (> y-max 1) y-max 1))
      #:x-label "x1"
      #:y-label "y"
      #:title "line model")

(send frame show #t)
