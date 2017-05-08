;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "SWidget.rkt")
(require "Handle.rkt")

;; ForTest:
(require "Model.rkt")

(provide Controller%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;; CONTROLLER                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Controller% is a (new Controller% [model Model<%>])

(define Controller%
  (class* SWidget% ()
    (super-new)

    ; the model to which the controllers will be connected
    (init-field model) ; Model<%>

    ; the handle which can make the controller draggable
    (init-field [handle (new Handle% [controller this])]) ; Handle<%>

    ; -> Int
    ; the x y coordinates of the center of the SWidget
    (inherit-field x)
    (inherit-field y)

    ; -> Bool
    ; is this selected? Default is false.
    (inherit-field selected?)

    ; -> PosInt
    ; saved mouse co-ordinates for accomplishing smooth drag.
    (inherit-field saved-mx)
    (inherit-field saved-my)

    ; -> Nat 
    ;; the boundrary of the field
    (inherit-field width)
    (inherit-field height)

    (send model register this)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: 1. if the mouse click is in the handle, 
    ;            then set! the handle's selected? flag as true.
    ;         2. else if the mouse click is in the controller, 
    ;            set! the controller's selected? flag as true.
    (define/override (after-button-down mx my)
      (cond[(send handle in-object? (- mx (origin-x)) (- my (origin-y)))
            (set-field! selected? handle true)
            (set! saved-mx mx)
            (set! saved-my my)]
           [(send this in-object? mx my)
            (set! selected? true)]))

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: if the handle is selected,
    ;         move the controller to a new location after drag
    (define/override (after-drag mx my)
      (cond [(get-field selected? handle)
             (set! x (+ x (- mx saved-mx)))
             (set! y (+ y (- my saved-my)))
             (set! saved-mx mx)
             (set! saved-my my)]))

    ; Integer Integer -> Void
    ; GIVEN: any Value
    ; EFFECT: set! the selected? flag for handle and the controller as false.
    (define/override (after-button-up mx my)
      (set! selected? false)
      (set-field! selected? handle false))

    ; -> Image
    ; RETURNS: a black frame with a handle placed on its top left
    (define/override (to-image)
      (overlay/align "left" "top"
                     (send handle to-image)
                     (to-bg-image)))

    ; -> Image
    ; RETURNS: the controller's specified data or view inside the frame
    (define/public (data-image) empty-image)

    ; -> Image
    ; RETURNS: the inner black frame with the data-image placed over it.
    (define (to-bg-image)
      (overlay (data-image)
               (rectangle width height "outline" "black")))

    ; -> Int
    ; RETURNS: the x y coordinates of the controller's top left corner
    (define (origin-x) (- x (/ width 2)))
    (define (origin-y) (- y (/ height 2)))

    ;; For Tests:
    (define/public (for-test:selected?) selected?) ;-> Boolean
    (define/public (for-test:x) x)                 ; -> Int
    (define/public (for-test:y) y)                 ; -> Int
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define test-img (overlay/align "left" "top"
                     (rectangle 10 10 "outline" "red")
                     (rectangle 200 150 "outline" "black")))

(begin-for-test
  (let*
      ((m1 (new Model%))
       (c0 (new Controller% [model m1])))
    
    (check-equal?
     (begin
       (send c0 after-button-down
             (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send c0 in-object? (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)))
     true)
    (check-equal?
     (begin
       (send c0 for-test:selected?))
     true)
    (check-equal?
     (begin
       (send c0 after-button-up (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
       (send c0 for-test:selected?))
     false)
    (check-equal?
     (begin
       (send c0 after-button-down 200 175)
       (send c0 for-test:selected?))
     false)
    (check-equal?
     (begin
       (send c0 after-drag 200 180)
       (send c0 for-test:x))
     300)
    (check-equal?
     (send c0 for-test:y)
     255)
    (check-equal?
     (send c0 data-image)
     empty-image)
    (check-equal?
     (send c0 to-image)
     test-img)
    ))