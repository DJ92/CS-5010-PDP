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

(provide XController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;; X CONTROLLER                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A XController% is a (new XController% [model Model<%>])

;; displays as an outline rectangle
;; with a particle bouncing inside only in X direction

;; the rectangle is draggable
;; dragging inside the controller can
;; change the x coordinate of the particle

(define XController%
  (class* Controller% ()
    (super-new)
    
    ; the model to which the controllers will be connected
    (inherit-field model) ; Model<%>
    
    ; -> Int
    ; the x y coordinates of the center of the SWidget
    (inherit-field x)
    (inherit-field y)

    ; -> Bool
    ; is this selected? Default is false.
    (inherit-field selected?)

    ; -> PosInt
    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; blocks's center.  Else any value.
    (inherit-field saved-mx)
    (inherit-field saved-my)

    ; -> Nat
    ;; the boundrary of the field
    (inherit-field width)
    (inherit-field height)

    (set! width 200) 
    (set! height 50)

    ; Nat Nat -> Boolean
    ; GIVEN: a mouse location
    ; RETURNS: true iff the mouse is in the model's boundrary
    (define/override (in-object? other-x other-y)
      (let [(width (get-field width model))]
        (and
          (<= (- x (/ width 2))  other-x (+ x (/ width 2)))
          (<= (- y (/ height 2)) other-y (+ y (/ height 2))))))

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: if the mouse is in the controller, 
    ;         set! selected? true and pause the particle
    (define/override (after-button-down mx my)
      (cond
        [(in-object? mx my)
          (set! selected? true)
          (set! saved-mx mx)
          (set! saved-my my)
          (set-field! paused? model true)]
        [else
          (super after-button-down mx my)]))

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: if the controller is selected,
    ;         move the particle using smooth drag
    ;         but only alter the x coordinate
    (define/override (after-drag mx my)
      (cond
        [selected?
          (send model update-particle-x
            (+ (send model return-particle-x) (- mx saved-mx)))
          (set! saved-mx mx)
          (set! saved-my my)]
        [else
          (super after-drag mx my)]))

    ; Integer Integer -> Void
    ; GIVEN: any Value
    ; EFFECT: set! the controller selected? false
    ;         and set the model unpaused
    (define/override (after-button-up mx my)
      (super after-button-up mx my)
      (set! selected? false)
      (set-field! paused? model false))
    
    ; -> Image
    ; RETURNS: The particle image
    (define particle-img
      (overlay (circle 2 "solid" "black")
               (circle 10 "solid" "red")))
    ; -> Image
    ; RETURNS: The stage for the particle
    (define bg-image
      (rectangle (get-field width model)
                 height
                 "outline"
                 "blue"))
    
    ; -> Image
    ; RETURNS: the particle placed on a blue rectangle
    (define/override (data-image)
      (overlay bg-image
               (place-image particle-img
                            (send model return-particle-x)
                            (/ height 2)
                            bg-image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define test-img
  (overlay (rectangle 150 50 "outline" "blue")
           (place-image
            (overlay (circle 2 "solid" "black")
                     (circle 10 "solid" "red"))
            80 25
            (rectangle 150 50 "outline" "blue"))))

(begin-for-test
  (let*
      ((m (new Model%))
       (x (new XController% [model m])))
    
    (check-equal?
     (begin
       (send x after-button-down
             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send x in-object? (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)))
     true)
    
    (check-equal?
     (begin
       (send x after-button-up (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send x after-drag 305 255)
       (send x for-test:selected?))
     false)
    
    (check-equal?
     (begin
       (send x after-button-down 200 150)
       (send x for-test:selected?))
     false)

    (check-equal?
     (begin
       (send x after-button-down
             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send x after-drag 305 255)
       (send m return-particle-x))
     80)

    (check-equal?
     (send m return-particle-y)
     50)
    
    (check-equal?
     (send x data-image)
     test-img)

    ))