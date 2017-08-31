#lang racket/base

(require racket/class)
(require racket/math)
(require racket/gui/base)
(require pict)
(require pict/color)
(require pict/shadow)

(provide thermo%
         thermo-demo)

(define black        (make-object color% 0 0 0))
(define dimmed-grey  (make-object color% 0 0 0 0.1))
(define green        (make-object color% 0 255 0))
(define dimmed-green (make-object color% 0 255 0 0.5))
(define papaya       (make-object color% 255 239 213))
(define crimson      (make-object color% 220 20 60))
(define sea-green    (make-object color% 32 178 170))
(define peach-puff   (make-object color% 255 218 185))
(define steele-blue  (make-object color% 70 130 180))

(define bad-color (make-object color% 255 0 0))
(define good-color (make-object color% 0 255 0))
(define face-color (make-object color% 255 255 0))
(define face-color2 (make-object color% 200 200 0))

(define face-brush (new brush% (color face-color)))
(define face-brush2 (new brush% (color face-color2)))

(define (apply-derived-colors!)
  (set! face-brush (new brush% (color face-color)))
  (set! face-brush2 (new brush% (color face-color2))))

(define (apply-regular-theme!)
  (set! bad-color (make-object color% 255 0 0))
  (set! good-color (make-object color% 0 255 0))
  (set! face-color (make-object color% 255 255 0))
  (set! face-color2 (make-object color% 200 200 0))
  (apply-derived-colors!))

(define (apply-console-theme!)
  (set! bad-color black)
  (set! good-color black)
  (set! face-color green)
  (set! face-color2 sea-green)
  (apply-derived-colors!))

(define (apply-colorful-theme!)
  (set! bad-color crimson)
  (set! good-color steele-blue)
  (set! face-color papaya)
  (set! face-color2 peach-puff)
  (apply-derived-colors!))

;; (apply-regular-theme!)
;; (apply-console-theme!)
(apply-colorful-theme!)

(define thermo%
  (class canvas%
    (init-field parent
                value
                min-value
                max-value
                (fixed-height 50))

    (super-new (parent parent)
               (min-width (* 2 fixed-height))
               (min-height fixed-height)
               (stretchable-height #f)
               (style '(transparent)))

    (define dc (send this get-dc))
    (send dc set-smoothing 'smoothed)

    (define width (send this get-width))
    (define height (send this get-height))

    (define gradient-brush
      (new brush% (gradient
                   (new linear-gradient%
                        (x0 (/ height 2))
                        (y0 0)
                        (x1 (- width height))
                        (y1 0)
                        (stops (list (list 0 bad-color)
                                     (list 1 good-color)))))))

    (define (gradient-brush!)
      (set! gradient-brush
        (new brush% (gradient
                     (new linear-gradient%
                          (x0 (/ height 2))
                          (y0 0)
                          (x1 (- width height))
                          (y1 0)
                          (stops (list (list 0 bad-color)
                                       (list 1 good-color))))))))

    (define/public (set-value! new-value)
      (unless (= new-value value)
        (set! value new-value)
        (send this refresh)))
    
    (define/override (on-paint)
      ;; draw rectangle
      (define height/2 (/ height 2))
      (define height/4 (/ height 4))
      (define height/8 (/ height 8))
      (send dc set-brush gradient-brush)
      (unless (negative? (- width height/2))
        (send dc draw-rectangle
              height/2 0
              (- width height) height))

      ;; draw face
      (send dc set-brush face-brush)
      (define cap-value (cond ((< value min-value) min-value)
                              ((> value max-value) max-value)
                              (else value)))
      (define val-coef (/ (- cap-value min-value) (- max-value min-value)))
      (define face-x (* (- width height) val-coef))
      (define face-y 0)
      (send dc draw-ellipse
            face-x face-y
            fixed-height fixed-height)
      (send dc set-brush face-brush2)
      (send dc draw-ellipse
            (+ face-x height/4) (+ face-y height/4)
            height/8 height/8)
      (send dc draw-ellipse
            (+ face-x height/2 height/8) (+ face-y height/4)
            height/8 height/8)
      (send dc draw-spline
            (+ face-x height/4) (+ face-y height/2 height/8)
            (+ face-x height/2) (+ face-y height/2 (* val-coef height/4))
            (+ face-x height/2 height/4) (+ face-y height/2 height/8)))

    (define/override (on-size new-width new-height)
      (unless (= new-width width)
        (set! width new-width)
        (gradient-brush!))
      (unless (= new-height height)
        (set! height new-height)
        (gradient-brush!))
      (send this refresh))))

(define (thermo-demo)
  (define frame (new frame%
                     (label "Testing thermometer")))
  (define layout (new vertical-panel%
                      (parent frame)
                      (border 10)
                      (spacing 10)))
  (define thermo (new thermo%
                      (parent layout)
                      (value 0)
                      (min-value -1)
                      (max-value 1)))
  (send frame show #t))


