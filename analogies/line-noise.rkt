#lang racket/gui

(require math)
(require math/distributions)
(require plot)

(require "simplot.rkt")
(require "poten.rkt")
(require "thermo.rkt")

(define (line-func w b)
  (lambda (x)
    (+ b (* w x))))

(define (line-func/noise w b dev)
  (lambda (x)
    (+  b (* w x) ((distribution-sample (normal-dist 0 dev))))))

(define (line-points w b)
  (lambda (xs)
    (map (line-func w b) xs)))

(define (line-points/noise w b dev)
  (lambda (x)
    (map (line-func/noise w b dev) xs)))

(define real-w 1)
(define real-b 0.5)
(define real-dev 0.1)

(define m 20)

(define line-mapper (line-points real-w real-b))

(define xs ((distribution-sample (uniform-dist 0 1)) m))
(define es ((distribution-sample (normal-dist 0 real-dev)) m))
(define ys (line-mapper xs))
(define ys/es (map + ys es))


(define (plot-ideal)
  (plot (list (points (map list xs ys)
                      #:color "red")
              (points (map list xs ys/es)
                      #:color "blue"))
        #:x-min -0.1 #:x-max 1.1
        #:y-min 0.0 #:y-max 1.6))

(define frame (new frame%
                   (label "line noisy model")
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
                          (label "")
                          (auto-resize #t)))

(define monitor (new horizontal-panel%
                     (parent plots)
                     (stretchable-height #f)))

(define buttons (new horizontal-panel%
                     (parent plots)
                     (stretchable-height #f)))

(define button-randomize (new button%
                              (parent buttons)
                              (label "randomize")
                              (stretchable-width #t)
                              (callback (lambda (b e) (randomize!)))))

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


(define plots-group (new horizontal-panel%
                         (parent plots)
                         (spacing 5)))

(define (randomize!)
  (set! es ((distribution-sample (normal-dist 0 real-dev)) m))
  (set! ys/es (map + ys es))
  (set! J (cost))
  (send thermo set-value! J)
  (send message-cost set-label (number->string J))
  (send plot-x-y/e set-source-points! (map cons xs ys/es)))

(define (reset!)
  (send control-w set-value! ((distribution-sample (uniform-dist -1 1))))
  (send control-b set-value! ((distribution-sample (uniform-dist -1 1))))
  (set! J (cost))
  (send thermo set-value! J)
  (send message-cost set-label (number->string J))
  (set! stop-algorithm? #f)
  (send button-stop enable #f))

(define stop-algorithm? #f)

(define w ((distribution-sample (uniform-dist -1 1))))
(define b ((distribution-sample (uniform-dist -1 1))))

(define (cost)
  (define ys* ((line-points w b) xs))
  (- (/ (apply + (map (lambda (y* y) (expt (- y* y) 2)) ys* ys/es)) m)))

(define J (cost))
(send message-cost set-label (number->string J))

(define thermo (new thermo%
                    (parent monitor)
                    (value J)
                    (min-value -0.5)
                    (max-value 0)))

(define (fx x) (+ b (* w x)))
(define (fx* x) (+ real-b (* real-w x)))

(define plot-x-y/e
  (new simplot%
       (parent plots-group)
       (source-points (map cons xs ys/es))
       (source-function fx)
       (expected-function fx*)))

(define control-w
  (new poten%
       (parent controls)
       (label "w")
       (notify-change (lambda (w*)
                        (set! w w*)
                        (set! J (cost))
                        (send plot-x-y/e refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value w)))

(define control-b
  (new poten%
       (parent controls)
       (label "b")
       (notify-change (lambda (b*)
                        (set! b b*)
                        (set! J (cost))
                        (send plot-x-y/e refresh)
                        (send thermo set-value! J)
                        (send message-cost set-label (number->string J))))
       (initial-value b)))

(send frame show #t)

(define (J-growth control)
  (define x (send control get-value))
  (define Jx J)
  (define x* (+ x 0.00001))
  (define Jx* (begin (send control set-value! x*) J))
  (send control set-value! x)
  (/ (- Jx* Jx) 0.00001))

(define (gradient-descent!)
  (define (loop learning-rate)
    (define dJw (J-growth control-w))
    (define dJb (J-growth control-b))
    (send control-w set-value! (+ w (* learning-rate dJw)))
    (send control-b set-value! (+ b (* learning-rate dJb)))
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
    (define new-w (+ w ((distribution-sample (uniform-dist -.05 .05)))))
    (define new-b (+ b ((distribution-sample (uniform-dist -.05 .05)))))
    (when (>= (prob control-w new-w T) ((distribution-sample (uniform-dist 0 1))))
      (send control-w set-value! new-w))
    (when (>= (prob control-b new-b T) ((distribution-sample (uniform-dist 0 1))))
      (send control-b set-value! new-b))
    (unless stop-algorithm?
      (sleep 0.1)
      (loop (+ i 1) (temp (+ i 1)))))
  (loop 1 (temp 1)))
