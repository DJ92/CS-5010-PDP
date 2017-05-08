;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "Controller.rkt")

;; ForTest:
(require "Model.rkt")

(provide VelocityController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;; VELOCITY CONTROLLER                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A VelocityController% is a (new VelocityController% [model Model<%>])

;; displays as an outline rectangle with text showing
;; the x y coordinates and velocity of the particle.

;; the rectangle is draggable

;; +,- increments or decrements the speed of the particle

(define VelocityController%
  (class* Controller% ()
    (super-new)

    (inherit-field model)  ; the model

    ; -> Nat
    ; the boundrary of the field
    (inherit-field width)
    (inherit-field height)

    ; -> Bool
    ; is this selected? Default is false.
    (inherit-field selected?)

    (set! width 200) ; width of the velocity controller
    (set! height 50) ; height of the velocity controller

    ; KeyEvent -> Void
    ; EFFECT:1. Hitting arrow key "left" "right"
    ;           to - or + the velocity in x direction
    ;        2. Hitting "up" "down" to - or + the velocity in y direction
    (define/override (after-key-event kev)
      (cond [selected?
             (cond
               [(key=? kev "up")
                (send model update-particle-vy
                      (- (send model return-particle-vy) 5))]
               [(key=? kev "down")
                (send model update-particle-vy
                      (+ (send model return-particle-vy) 5))]
               [(key=? kev "left")
                (send model update-particle-vx
                      (- (send model return-particle-vx) 5))]
               [(key=? kev "right")
                (send model update-particle-vx
                      (+ (send model return-particle-vx) 5))]
               )]))

    ; -> String
    ; RETURNS: if the handle is selected, return red, else black
    (define (current-color)
      (if selected? "red" "black"))

    ; -> Image
    ; RETURNS: an image showing the x y coordinates
    ;          and the velocity of this particle 
    (define/override (data-image)
      (above
        (text "Arrow keys change velocity" 10 (current-color))
        (text (format "X = ~a Y = ~a"
                      (send model return-particle-x)
                      (send model return-particle-y))
              12
              (current-color))
        (text (format "VX = ~a VY = ~a"
                      (send model return-particle-vx)
                      (send model return-particle-vy))
              12
              (current-color))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define test-img
  (above
   (text "Arrow keys change velocity" 10 "red")
   (text "X = 75 Y = 50" 12 "red")
   (text "VX = 0 VY = 0" 12 "red")))

(define test-img1
  (above
   (text "Arrow keys change velocity" 10 "black")
   (text "X = 75 Y = 50" 12 "black")
   (text "VX = 0 VY = 0" 12 "black")))

(begin-for-test
  (let*
      ((m (new Model%))
       (p (new VelocityController% [model m])))
    
    (check-equal?
     (begin
       (send p after-button-down
             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send p after-key-event "up")
       (send m return-particle-vy))
     -5)
    (check-equal?
     (begin
       (send p after-key-event "down")
       (send m return-particle-vy))
     0)
    (check-equal?
     (begin
       (send p after-key-event "left")
       (send m return-particle-vx))
     -5)
    (check-equal?
     (begin
       (send p after-key-event "right")
       (send m return-particle-vx))
     0)

    (check-equal?
     (send p data-image)
     test-img)
    
    (check-equal?
     (begin
       (send p after-button-up
             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send p data-image))
     test-img1) 

    ))