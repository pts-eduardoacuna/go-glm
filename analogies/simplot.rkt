#lang racket/base

(require racket/list)
(require racket/class)
(require racket/gui/base)
(require plot)

(provide simplot%
         demo)

(define font       (make-object font% 8 'modern))
(define brush      (new brush% (style 'transparent)))
(define line-pen   (new pen% (width 1)))
(define light-grey (make-object color% 0 0 0 0.05))
(define grid-pen   (new pen% (width 0) (style 'dot) (color light-grey)))
(define dot-pen    (new pen% (width 5)))


(define tks (linear-ticks #:number 2 #:divisors '(1 2)))

(define (make-ticks lo hi)
  (map (lambda (t)
         (list (pre-tick-value t) (tick-label t) (pre-tick-major? t)))
       (ticks-generate tks lo hi)))

(define simplot%
  (class canvas%
    (init-field parent
                source-data
                (x-min -1)
                (x-max +1)
                (y-min -1)
                (y-max +1)
                (samples 20)
                (antialiasing? #t))

    (super-new (parent            parent)
               (style     '(transparent))
               (min-width             50)
               (min-height            50))


    ;;;;;;;;;;;;;;;;
    ;; PLOT STATE ;;
    ;;;;;;;;;;;;;;;;

    (define dc (send this get-dc))
    (send dc set-smoothing (if antialiasing? 'smoothed 'unsmoothed))
    (send dc set-font      font)
    (send dc set-brush     brush)

    (define width   (send this get-width))
    (define height  (send this get-height))
    (define x-span  (- x-max x-min))
    (define y-span  (- y-max y-min))
    (define x-proj  (/ width x-span))
    (define y-proj  (/ height (- y-span)))
    (define step    (/ x-span samples))
    (define xs      (range x-min x-max step))
    (define x-ticks (make-ticks x-min x-max))
    (define y-ticks (make-ticks y-min y-max))


    ;;;;;;;;;;;;;;;;;
    ;; MOUSE STATE ;;
    ;;;;;;;;;;;;;;;;;

    (define mouse-x #f)
    (define mouse-y #f)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; AUXILIARY PROCEDURES ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (set-bounds! x-min* x-max* y-min* y-max*)
      (unless (and (= x-min x-min*)
                   (= x-max x-max*))
        (set! x-span  (- x-max* x-min*))
        (set! step    (/ x-span samples))
        (set! x-proj  (/ width x-span))
        (set! xs      (range x-min* x-max* step))
        (set! x-ticks (make-ticks x-min* x-max*)))
      (unless (= x-min x-min*)
        (set! x-min x-min*))
      (unless (= x-max x-max*)
        (set! x-max x-max*))
      (unless (and (= y-min y-min*)
                   (= y-max y-max*))
        (set! y-span  (- y-max* y-min*))
        (set! y-proj  (/ height (- y-span)))
        (set! y-ticks (make-ticks y-min* y-max*)))
      (unless (= y-min y-min*)
        (set! y-min y-min*))
      (unless (= y-max y-max*)
        (set! y-max y-max*))
      (send this refresh))

    (define (pan-view! dx dy)
      (define step-x (* dx x-span))
      (define step-y (* dy y-span))
      (set-bounds! (- x-min step-x)
                   (- x-max step-x)
                   (- y-min step-y)
                   (- y-max step-y)))

    (define (zoom-view! dx dy)
      (define step-x (* dx x-span))
      (define step-y (* dy y-span))
      (set-bounds! (+ x-min step-x)
                   (- x-max step-x)
                   (+ y-min step-y)
                   (- y-max step-y)))

    ;;;;;;;;;;;;;;;;;;
    ;; PLOT METHODS ;;
    ;;;;;;;;;;;;;;;;;;

    (define/public (get-samples)
      samples)

    (define/public (set-samples! samples*)
      (unless (or (= samples samples*)
                  (not (exact-positive-integer? samples*)))
        (set! step    (/ x-span samples*))
        (set! xs      (range x-min x-max step))
        (set! samples samples*)
        (send this refresh)))

    (define/public (get-antialiasing)
      antialiasing?)

    (define/public (set-antialiasing! antialiasing?*)
      (unless (or (eq? antialiasing? antialiasing?*)
                  (not (boolean? antialiasing?*)))
        (set! antialiasing? antialiasing?*)
        (send dc set-smoothing (if antialiasing? 'smoothed 'unsmoothed))
        (send this refresh)))

    (define/public (get-source-data)
      source-data)

    (define/public (set-source-data! source-data*)
      (unless (or (eq? source-data source-data*)
                  (and (not (procedure? source-data*))
                       (not (pair? source-data*))))
        (set! source-data source-data*)
        (send this refresh)))

    ;;;;;;;;;;;;;;;;;;;;
    ;; CANVAS METHODS ;;
    ;;;;;;;;;;;;;;;;;;;;

    (define/override (on-char e)
      (case (send e get-key-code)
        ;; camera pan
        ((up)    (pan-view! +0.00 +0.05))
        ((down)  (pan-view! +0.00 -0.05))
        ((left)  (pan-view! -0.05 +0.00))
        ((right) (pan-view! +0.05 +0.00))
        ;; camera zoom
        ((#\+ #\=) (cond ((send e get-meta-down)    (zoom-view! +0.05 +0.00))
                         ((send e get-control-down) (zoom-view! +0.00 +0.05))
                         (else                      (zoom-view! +0.05 +0.05))))
        ((#\- #\_) (cond ((send e get-meta-down)    (zoom-view! -0.05 +0.00))
                         ((send e get-control-down) (zoom-view! +0.00 -0.05))
                         (else                      (zoom-view! -0.05 -0.05))))
        ;; sampling control
        ((#\] #\} #\a) (set-samples! (if (<= samples 2) samples (- samples 1))))
        ((#\[ #\{ #\s) (set-samples! (+ samples 1)))))

    (define/override (on-event e)
      (cond ((send e dragging?)
             (define x (send e get-x))
             (define y (send e get-y))
             (pan-view! (/ (- x mouse-x) 1000) (/ (- mouse-y y) 1000))
             (set! mouse-x x)
             (set! mouse-y y))
            ((send e button-down?)
             (set! mouse-x (send e get-x))
             (set! mouse-y (send e get-y)))
            ((send e button-up?)
             (set! mouse-x #f)
             (set! mouse-y #f))))

    (define/override (on-paint)
      ;; paint axis
      (send dc set-pen line-pen)
      (when (<= x-min 0 x-max)
        (send dc draw-line
              (* x-proj (- x-min))
              (* y-proj (- y-min y-max))
              (* x-proj (- x-min))
              0)
        (unless (< height 150)
          (for-each (lambda (t)
                      (send dc draw-text (second t)
                            (* x-proj (- x-min))
                            (* y-proj (- (first t) y-max))))
                    y-ticks)))
      (when (<= y-min 0 y-max)
        (send dc draw-line
              0
              (* y-proj (- y-max))
              (* x-proj (- x-max x-min))
              (* y-proj (- y-max)))
        (unless (< width 150)
         (for-each (lambda (t)
                     (send dc draw-text (second t)
                           (* x-proj (- (first t) x-min))
                           (* y-proj (- y-max))))
                   x-ticks)))
      (send dc set-pen grid-pen)
      (for-each (lambda (t)
                  (when (third t)
                    (send dc draw-line
                          0
                          (* y-proj (- (first t) y-max))
                          (* x-proj x-span)
                          (* y-proj (- (first t) y-max)))))
                y-ticks)

      (for-each (lambda (t)
                  (when (third t)
                    (send dc draw-line
                          (* x-proj (- (first t) x-min))
                          (* y-proj (- y-span))
                          (* x-proj (- (first t) x-min))
                          0)))
                x-ticks)

      ;; paint data
      (cond ((procedure? source-data)
             (define path (new dc-path%))
             (send dc set-pen line-pen)
             (send path move-to
                   0
                   (* y-proj (- (source-data x-min) y-max)))
             (for-each (lambda (x)
                         (send path line-to
                               (* x-proj (- x x-min))
                               (* y-proj (- (source-data x) y-max))))
                       xs)
             (send dc draw-path path))
            ((pair? source-data)
             (send dc set-pen dot-pen)
             (for-each (lambda (point)
                         (send dc draw-point
                               (* x-proj (- (car point) x-min))
                               (* y-proj (- (cdr point) y-max))))
                       source-data))))

    ;; refresh state
    (define/override (on-size new-width new-height)
      (unless (= new-width width)
        (set! x-proj (/ new-width x-span))
        (set! width new-width))
      (unless (= new-height height)
        (set! y-proj (/ new-height (- y-span)))
        (set! height new-height))
      (send this refresh))))

(define (demo)
  (define frame (new frame% [label "Testing simplot"]))
  (define panel (new vertical-panel%
                     [parent frame]
                     [spacing 10]
                     [border 10]))
  (define (logistic z)
    (/ 1.0 (+ 1.0 (exp (- z)))))
  (define (make-line m b)
    (lambda (x)
      (+ (* m x) b)))
  (define points
    (build-list 20 (lambda (i)
                     (let ((z (/ (- i 10.0) 2.0)))
                       (cons z (logistic z))))))
  (define funct-plot (new simplot%
                          [parent panel]
                          [source-data logistic]
                          [x-min -6.0] [x-max +6.0]
                          [y-min -0.2] [y-max +1.2]))
  (define point-plot (new simplot%
                          [parent panel]
                          [source-data points]
                          [x-min -6.0] [x-max +6.0]
                          [y-min -0.2] [y-max +1.2]))
  (send frame show #t))
