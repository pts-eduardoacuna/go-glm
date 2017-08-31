#lang racket/gui

(require math)
(require math/distributions)

(require "simplot.rkt")
(require "poten.rkt")
(require "thermo.rkt")

(define (plane-func w1 w2 w3 b)
  (lambda (x1 x2 x3)
    (+ b (* w1 x1) (* w2 x2) (* w3 x3))))

(define (plane-points w1 w2 w3 b)
  (lambda (x1s x2s x3s)
    (map (plane-func w1 w2 w3 b) x1s x2s x3s)))


(define real-w1 1)
(define real-w2 -1/2)
(define real-w3 1/3)
(define real-b 0.5)
(define plane-mapper (plane-points real-w1 real-w2 real-w3 real-b))
(define samp (distribution-sample (uniform-dist 0 1)))
(define x1s (samp 20))
(define x2s (samp 20))
(define x3s (samp 20))
(define ys  (plane-mapper x1s x2s x3s))
(define m   (length ys))


(define frame (new frame%
                   (label "hyperplane model")
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

(define message-cost (new message%
                          (parent plots)
                          (label  "")
                          ;;(stretchable-width #t)
                          (auto-resize #t)))

(define monitor (new horizontal-panel%
                     (parent plots)
                     (stretchable-height #f)))

(define buttons (new horizontal-panel%
                     (parent plots)
                     (stretchable-height #f)))

(define button-reset (new button%
                          (parent buttons)
                          (label "reset")
                          (stretchable-width #t)
                          (callback (lambda (b e) (reset!)))))

(define button-smooth (new button%
                           (parent buttons)
                           (label "smooth algorithm")
                           (stretchable-width #t)
                           (callback (lambda (b e)
                                       (set! stop-algorithm? #f)
                                       (send button-stop enable #t)
                                       (thread gradient-descent!)))))

(define button-random (new button%
                           (parent buttons)
                           (label "random algorithm")
                           (stretchable-width #t)
                           (callback (lambda (b e)
                                       (set! stop-algorithm? #f)
                                       (send button-stop enable #t)
                                       (thread simulated-annealing!)))))

(define button-stop (new button%
                         (parent buttons)
                         (enabled #f)
                         (label "stop")
                         (stretchable-width #t)
                         (callback (lambda (b e)
                                     (set! stop-algorithm? #t)
                                     (send button-stop enable #f)))))
                          

(define plots-group-1 (new horizontal-panel%
                           (parent plots)
                           (spacing 5)))

(define plots-group-2 (new horizontal-panel%
                           (parent plots)
                           (spacing 5)))

(define (reset!)
  (send control-w1 set-value! ((distribution-sample (uniform-dist -1 1))))
  (send control-w2 set-value! ((distribution-sample (uniform-dist -1 1))))
  (send control-w3 set-value! ((distribution-sample (uniform-dist -1 1))))
  (send control-b  set-value! ((distribution-sample (uniform-dist -1 1))))
  (set! J (cost))
  (send thermo set-value! J)
  (send message-cost set-label (number->string J))
  (set! stop-algorithm? #f)
  (send button-stop enable #f))

(define stop-algorithm? #f)

(define (J-growth control)
  (define x (send control get-value))
  (define Jx J)
  (define x* (+ x 0.00001))
  (define Jx* (begin (send control set-value! x*) J))
  (send control set-value! x)
  (/ (- Jx* Jx) 0.00001))

(define (gradient-descent!)
  (define (loop learning-rate)
    (define dJw1 (J-growth control-w1))
    (define dJw2 (J-growth control-w2))
    (define dJw3 (J-growth control-w3))
    (define dJb  (J-growth control-b))
    (send control-w1 set-value! (+ w1 (* learning-rate dJw1)))
    (send control-w2 set-value! (+ w2 (* learning-rate dJw2)))
    (send control-w3 set-value! (+ w3 (* learning-rate dJw3)))
    (send control-b  set-value! (+ b  (* learning-rate dJb)))
    (unless stop-algorithm? 
      (sleep 0.1)
      (loop learning-rate)))
  (loop 0.1))

(define (simulated-annealing!)
  (define (temp i)
    (exp (- (/ i 69))))
  (define (prob control-w w* T)
    (define w (send control-w get-value))
    (define Jw J)
    (define Jw* (begin (send control-w set-value! w*) J))
    (send control-w set-value! w)
    (if (> Jw* Jw) 1.0 T))
  (define (loop i T)
    (define new-w1 (+ w1 ((distribution-sample (uniform-dist -.05 .05)))))
    (define new-w2 (+ w2 ((distribution-sample (uniform-dist -.05 .05)))))
    (define new-w3 (+ w3 ((distribution-sample (uniform-dist -.05 .05)))))
    (define new-b  (+ b  ((distribution-sample (uniform-dist -.05 .05)))))
    (when (>= (prob control-w1 new-w1 T) ((distribution-sample (uniform-dist 0 1))))
      (send control-w1 set-value! new-w1))
    (when (>= (prob control-w2 new-w2 T) ((distribution-sample (uniform-dist 0 1))))
      (send control-w2 set-value! new-w2))
    (when (>= (prob control-w3 new-w3 T) ((distribution-sample (uniform-dist 0 1))))
      (send control-w3 set-value! new-w3))
    (when (>= (prob control-b new-b T) ((distribution-sample (uniform-dist 0 1))))
      (send control-b set-value! new-b))
    (unless stop-algorithm?
      (sleep 0.1)
      (loop (+ i 1) (temp (+ i 1)))))
  (loop 1 (temp 1)))

(define w1 ((distribution-sample (uniform-dist -1 1))))
(define w2 ((distribution-sample (uniform-dist -1 1))))
(define w3 ((distribution-sample (uniform-dist -1 1))))
(define b  ((distribution-sample (uniform-dist -1 1))))

(define (cost)
  (define ys* ((plane-points w1 w2 w3 b) x1s x2s x3s))
  (- (/ (apply + (map (lambda (y* y) (expt (- y* y) 2)) ys* ys)) m)))

(define J (cost))
(send message-cost set-label (number->string J))

(define thermo
  (new thermo%
       (parent monitor)
       (value J)
       (min-value -0.5)
       (max-value 0)))

(define (fx1 x1) (+ b (* w1 x1)))
(define (fx2 x2) (+ b (* w2 x2)))
(define (fx3 x3) (+ b (* w3 x3)))

(define (gx1 x1) (/ (+ b (* w1 x1)) (- w2)))
(define (gx2 x2) (/ (+ b (* w2 x2)) (- w3)))
(define (gx3 x3) (/ (+ b (* w3 x3)) (- w1)))

(define (fx1* x1) (+ real-b (* real-w1 x1)))
(define (fx2* x2) (+ real-b (* real-w2 x2)))
(define (fx3* x3) (+ real-b (* real-w3 x3)))

(define (gx1* x1) (/ (+ real-b (* real-w1 x1)) (- real-w2)))
(define (gx2* x2) (/ (+ real-b (* real-w2 x2)) (- real-w3)))
(define (gx3* x3) (/ (+ real-b (* real-w3 x3)) (- real-w1)))

(define plot-x1-y
  (new simplot%
       (parent plots-group-1)
       (source-points (map cons x1s ys))
       (source-function fx1)
       (expected-function fx1*)))

(define plot-x2-y
  (new simplot%
       (parent plots-group-1)
       (source-points (map cons x2s ys))
       (source-function fx2)
       (expected-function fx2*)))

(define plot-x3-y
  (new simplot%
       (parent plots-group-1)
       (source-points (map cons x3s ys))
       (source-function fx3)
       (expected-function fx3*)))

(define plot-x1-x2
  (new simplot%
       (parent plots-group-2)
       (source-points (map cons x1s x2s))
       (source-function gx1)
       (expected-function gx1*)))

(define plot-x2-x3
  (new simplot%
       (parent plots-group-2)
       (source-points (map cons x2s x3s))
       (source-function gx2)
       (expected-function gx2*)))

(define plot-x3-x1
  (new simplot%
       (parent plots-group-2)
       (source-points (map cons x3s x1s))
       (source-function gx3)
       (expected-function gx3*)))

(define control-w1
  (new poten%
       (parent controls)
       (label "w1")
       (notify-change (lambda (w1*)
                        (set! w1 w1*)
                        (set! J (cost))
                        (send plot-x1-y refresh)
                        (send plot-x1-x2 refresh)
                        (send plot-x3-x1 refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value w1)))

(define control-w2
  (new poten%
       (parent controls)
       (label "w2")
       (notify-change (lambda (w2*)
                        (set! w2 w2*)
                        (set! J (cost))
                        (send plot-x2-y refresh)
                        (send plot-x2-x3 refresh)
                        (send plot-x1-x2 refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value w2)))

(define control-w3
  (new poten%
       (parent controls)
       (label "w3")
       (notify-change (lambda (w3*)
                        (set! w3 w3*)
                        (set! J (cost))
                        (send plot-x3-y refresh)
                        (send plot-x3-x1 refresh)
                        (send plot-x2-x3 refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value w3)))

(define control-b
  (new poten%
       (parent controls)
       (label "b")
       (notify-change (lambda (b*)
                        (set! b b*)
                        (set! J (cost))
                        (send plot-x1-y refresh)
                        (send plot-x2-y refresh)
                        (send plot-x3-y refresh)
                        (send plot-x1-x2 refresh)
                        (send plot-x2-x3 refresh)
                        (send plot-x3-x1 refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value b)))

(send frame show #t)
