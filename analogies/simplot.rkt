#lang racket/base

(require racket/list)
(require racket/class)
(require racket/gui/base)
(require plot)

(provide simplot%
         simplot-demo)

(define black        (make-object color% 0 0 0))
(define dimmed-grey  (make-object color% 0 0 0 0.1))
(define green        (make-object color% 0 255 0))
(define dimmed-green (make-object color% 0 255 0 0.5))
(define papaya       (make-object color% 255 239 213))
(define crimson      (make-object color% 220 20 60))
(define sea-green    (make-object color% 32 178 170))
(define peach-puff   (make-object color% 255 218 185))
(define steele-blue  (make-object color% 70 130 180))

(define font       (make-object font% 8 'modern))
(define brush      (new brush% (style 'transparent)))
(define line-pen   (new pen% (width 1)))
(define grid-pen   (new pen% (width 0) (style 'dot) (color dimmed-grey)))
(define dot-pen    (new pen% (width 5)))
(define background #f)
(define text       black)

(define (apply-regular-theme!)
  (set! background #f)
  (set! text       black)
  (set! line-pen   (new pen% (width 1) (style 'solid) (color black)))
  (set! grid-pen   (new pen% (width 0) (style 'dot)   (color dimmed-grey)))
  (set! dot-pen    (new pen% (width 5) (style 'solid) (color black))))

(define (apply-console-theme!)
  (set! background black)
  (set! text       green)
  (set! line-pen   (new pen% (width 1) (style 'solid) (color green)))
  (set! grid-pen   (new pen% (width 0) (style 'dot)   (color dimmed-green)))
  (set! dot-pen    (new pen% (width 5) (style 'solid) (color green))))

(define (apply-colorful-theme!)
  (set! background papaya)
  (set! text       crimson)
  (set! line-pen   (new pen% (width 1) (style 'solid) (color sea-green)))
  (set! grid-pen   (new pen% (width 0) (style 'dot)   (color peach-puff)))
  (set! dot-pen    (new pen% (width 5) (style 'solid) (color steele-blue))))

;; (apply-regular-theme!)
;; (apply-console-theme!)
(apply-colorful-theme!)

(define tks
  ;; Uncomment next line and comment the one that follows it for better performance
  ;; (linear-ticks #:number 2 #:divisors '(1 2))
  (linear-ticks))

(define (make-ticks lo hi)
  (map (lambda (t)
         (list (pre-tick-value t) (tick-label t) (pre-tick-major? t)))
       (ticks-generate tks lo hi)))

(define simplot%
  (class canvas%
    (init-field parent
                (source-points   #f)
                (source-function #f)
                (x-min -1)
                (x-max +1)
                (y-min -1)
                (y-max +1)
                (samples 20)
                (antialiasing? #t)
                (grid? #t)
                (ticks? #t)
                (axis? #t))

    (super-new (parent     parent)
               (style      (if background null '(transparent)))
               (min-width  50)
               (min-height 50))

    (when background
      (send this set-canvas-background background))

    ;;;;;;;;;;;;;;;;
    ;; PLOT STATE ;;
    ;;;;;;;;;;;;;;;;

    (define dc (send this get-dc))
    (send dc set-smoothing (if antialiasing? 'smoothed 'unsmoothed))
    (send dc set-font      font)
    (send dc set-brush     brush)
    (send dc set-text-foreground text)

    (define width   (send this get-width))
    (define height  (send this get-height))
    (define x-span  (- x-max x-min))
    (define y-span  (- y-max y-min))
    (define x-proj  (/ width x-span))
    (define y-proj  (/ height (- y-span)))
    (define step    (/ x-span samples))
    (define xs      (range x-min (+ x-max step) step))
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
        (set! xs      (range x-min* (+ x-max* step) step))
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

    (define/public (get-grid)
      grid?)

    (define/public (set-grid! grid?*)
      (unless (or (eq? grid? grid?*)
                  (not (boolean? grid?*)))
        (set! grid? grid?*)
        (send this refresh)))

    (define/public (get-axis)
      axis?)

    (define/public (set-axis! axis?*)
      (unless (or (eq? axis? axis?*)
                  (not (boolean? axis?*)))
        (set! axis? axis?*)
        (send this refresh)))

    (define/public (get-ticks)
      ticks?)

    (define/public (set-ticks! ticks?*)
      (unless (or (eq? ticks? ticks?*)
                  (not (boolean? ticks?*)))
        (set! ticks? ticks?*)
        (send this refresh)))
    
    (define/public (get-samples)
      samples)

    (define/public (set-samples! samples*)
      (unless (or (= samples samples*)
                  (not (exact-positive-integer? samples*)))
        (set! step    (/ x-span samples*))
        (set! xs      (range x-min (+ x-max step) step))
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

    (define/public (get-source-points)
      source-points)

    (define/public (set-source-points! source-points*)
      (unless (or (eq? source-points source-points*)
                  (and (not (vector? source-points*))
                       (not (pair? source-points*))
                       (not (boolean? source-points*))))
        (set! source-points source-points*)
        (send this refresh)))

    (define/public (get-source-function)
      source-function)

    (define/public (set-source-function! source-function*)
      (unless (or (eq? source-function source-function*)
                  (and (not (procedure? source-function*))
                       (not (boolean? source-function*))))
        (set! source-function source-function*)
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
        ((#\[ #\{) (set-samples! (if (<= samples 2) samples (- samples 1))))
        ((#\] #\}) (set-samples! (+ samples 1)))
        ;; features control
        ((#\a) (set-antialiasing! (not antialiasing?)))
        ((#\t) (set-ticks! (not ticks?)))
        ((#\g) (set-grid! (not grid?)))
        ((#\l) (set-axis! (not axis?)))))

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
        (unless (not axis?)
         (send dc draw-line
               (* x-proj (- x-min))
               (* y-proj (- y-min y-max))
               (* x-proj (- x-min))
               0))
        (unless (or (< height 150) (not ticks?))
          (for-each (lambda (t)
                      (send dc draw-text (second t)
                            (* x-proj (- x-min))
                            (* y-proj (- (first t) y-max))))
                    y-ticks)))
      (when (<= y-min 0 y-max)
        (unless (not axis?)
          (send dc draw-line
                0
                (* y-proj (- y-max))
                (* x-proj (- x-max x-min))
                (* y-proj (- y-max))))
        (unless (or (< width 150) (not ticks?))
         (for-each (lambda (t)
                     (send dc draw-text (second t)
                           (* x-proj (- (first t) x-min))
                           (* y-proj (- y-max))))
                   x-ticks)))
      (when grid?
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
                  x-ticks))

      ;; paint data
      (when source-function
        (define path (new dc-path%))
        (send dc set-pen line-pen)
        (send path move-to
              0
              (* y-proj (- (source-function x-min) y-max)))
        (for ((x xs))
          (send path line-to
                (* x-proj (- x x-min))
                (* y-proj (- (source-function x) y-max))))
        (send dc draw-path path))

      (when source-points
        (send dc set-pen dot-pen)
        (for ((point source-points))
          (send dc draw-point
                (* x-proj (- (car point) x-min))
                (* y-proj (- (cdr point) y-max))))))

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
  (define p (new simplot%
                 (parent panel)
                 (source-function logistic)
                 (source-points points)
                 [x-min -6.0] [x-max +6.0]
                 [y-min -0.2] [y-max +1.2]))
  (send frame show #t))
