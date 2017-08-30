#lang racket/base

(require racket/class)
(require racket/math)
(require racket/gui/base)
(require pict)
(require pict/color)
(require pict/shadow)

(provide poten%
         poten-demo)

(define black        (make-object color% 0 0 0))       ; #000000
(define dimmed-grey  (make-object color% 0 0 0 0.1))   ; #000000
(define green        (make-object color% 0 255 0))     ; #00FF00
(define dimmed-green (make-object color% 0 255 0 0.5)) ; #00FF00
(define papaya       (make-object color% 255 239 213)) ; #FFEFD5
(define crimson      (make-object color% 220 20 60))   ; #DC143C
(define sea-green    (make-object color% 32 178 170))  ; #20B2AA
(define peach-puff   (make-object color% 255 218 185)) ; #FFDAB9
(define steele-blue  (make-object color% 70 130 180))  ; #4682B4
(define bored-grey1  (make-object color% 120 120 115)) ; #787873
(define bored-grey2  (make-object color% 227 227 226)) ; #E3E3E2
(define bored-grey3  (make-object color% 217 217 216)) ; #D9D9D8

(define border-color        (make-object color% 120 120 115))
(define background-main     (make-object color% 227 227 226))
(define background-secn     (make-object color% 217 217 216))
(define drk-border-color    (scale-color 0.9 border-color))
(define drk-background-main (scale-color 0.9 background-main))
(define drk-background-secn (scale-color 0.9 background-secn))
(define lgh-background-main (scale-color 2.0 background-main))
(define lgh-background-secn (scale-color 2.0 background-secn))

(define (apply-derived-colors!)
  (set! drk-border-color    (scale-color 0.9 border-color))
  (set! drk-background-main (scale-color 0.9 background-main))
  (set! drk-background-secn (scale-color 0.9 background-secn))
  (set! lgh-background-main (scale-color 2.0 background-main))
  (set! lgh-background-secn (scale-color 2.0 background-secn)))

(define (apply-regular-theme!)
  (set! border-color        bored-grey1)
  (set! background-main     bored-grey2)
  (set! background-secn     bored-grey3)
  (apply-derived-colors!))

(define (apply-console-theme!)
  (set! border-color    green)
  (set! background-main black)
  (set! background-secn dimmed-green)
  (apply-derived-colors!))

(define (apply-colorful-theme!)
  (set! border-color    crimson)
  (set! background-main papaya)
  (set! background-secn steele-blue)
  (apply-derived-colors!))

;; (apply-regular-theme!)
;; (apply-console-theme!)
(apply-colorful-theme!)

(define poten-canvas%
  (class canvas%
    (init-field parent initial-angle notify-change)
    (super-new (parent parent)
               (min-width 38)
               (min-height 38)
               (stretchable-width #f)
               (stretchable-height #f)
               (style '(transparent)))

    (define dc (send this get-dc))
    (define factor-fast 0.1)
    (define factor-normal 0.05)
    (define factor-slow 0.001)
    (define factor factor-normal)
    (define angle initial-angle)
    (define hovered? #f)
    (define y 0)
    (define frozen-y #f)
    (define frozen-angle #f)
    
    ;; set up DC
    (send dc set-smoothing 'smoothed)

    ;; compute the pict of the potentiometer
    (define (get-pict clicked? hover? angle)
      (inset
       (shadow
        (rotate
         (ct-superimpose
          (disk 30
                #:color (cond (clicked? lgh-background-main)
                              (hover? drk-background-main)
                              (else background-main))
                #:border-color (if hover?
                                   drk-border-color
                                   border-color))
          (cc-superimpose (blank 15)
                          (disk 7
                                #:color (cond (clicked? lgh-background-secn)
                                              (hover? drk-background-secn)
                                              (else background-secn)))))
         angle)
        6)
       6))

    ;; handle keyboard events
    (define/override (on-char e)
      (case (send e get-key-code)
        ((control) (set! factor factor-fast))
        ((shift)   (set! factor factor-slow))
        ((release)
         (case (send e get-key-release-code)
           ((control shift) (set! factor factor-normal)))))
      (send this refresh))

    ;; handle mouse events
    (define/override (on-event e)
      (when (send e entering?)
        (set! hovered? #t))
      (when (send e leaving?)
        (set! hovered? #f))
      (when (send e moving?)
        (set! y (send e get-y)))
      (unless (send e get-left-down)
        (set! frozen-y #f)
        (set! frozen-angle #f))
      (when (and (not frozen-y) (not frozen-angle) (send e get-left-down))
        (set! frozen-y (send e get-y))
        (set! frozen-angle angle))
      (when (and frozen-y frozen-angle (send e get-left-down))
        (set-angle (+ frozen-angle (* factor (- y frozen-y)))))
      (when (send e get-right-down)
        (set! frozen-y #f)
        (set! frozen-angle #f)
        (set-angle initial-angle))
      (send this refresh))

    ;; handle painting
    (define/override (on-paint)
      (define pict (get-pict frozen-y hovered? angle))
      (define cx (/ (pict-width pict) 2.0))
      (define cy (/ (pict-height pict) 2.0))
      (draw-pict pict dc (- 19 cx) (- 19 cy)))

    (define/public (get-angle) angle)

    (define/public (set-angle theta)
      (set! angle theta)
      (notify-change theta))))

(define poten%
  (class vertical-panel%
    (init-field parent label notify-change initial-value)
    (super-new (parent parent))

    (define (angle-changed theta)
      (define x (angle->value theta))
      (unless (= x value)
        (set-value x)))

    (define name-message (new message%
                              (parent this)
                              (label label)))
    (define canvas (new poten-canvas%
                        (parent this)
                        (initial-angle (value->angle initial-value))
                        (notify-change angle-changed)))
    (define value-message (new message%
                               (parent this)
                               (label (real->decimal-string initial-value))
                               (auto-resize #t)))

    (define value initial-value)

    (define/public (get-state) value)

    (define/public (set-value x)
      (set! value x)
      (send value-message set-label (real->decimal-string x))
      (send canvas set-angle (value->angle x))
      (notify-change x))))

(define (value->angle x)
  (- (* x (/ (* 2 pi) 10.0))))

(define (angle->value theta)
  (- (* theta (/ 10.0 (* 2 pi)))))

(define (poten-demo)
  (define frame (new frame%
                     (label "Testing potentiometer")))
  (define message (new message%
                       (parent frame)
                       (auto-resize #t)
                       (label "unmoved")))
  (define (update-message x)
    (send message set-label (format "value at: ~a" x)))
  (define poten (new poten%
                     (parent frame)
                     (label "Move me")
                     (notify-change update-message)
                     (initial-value 3.0)))
  (send frame show #t))
