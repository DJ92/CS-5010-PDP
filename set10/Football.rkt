;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")

(provide Football%
         make-football)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTANTS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIMENSIONS OF THE CANVAS
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

;; TARGET INITIAL CO-ORDINATES
(define TARGET-INITIAL-X HALF-CANVAS-WIDTH)
(define TARGET-INITIAL-Y HALF-CANVAS-HEIGHT)

;; MOUSE EVENTS
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")

;; ANY OTHER EVENT
(define OTHER-EVENT "enter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; EXAMPLES: refer test cases

(define (make-football x y)
  (new Football% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FOOTBALL

;; A Football appear at the center of the target
;; The football gets smaller with every tick until it reaches size 0.
;; they're selectable and draggable.

;; A Football is a (new Football% [x Integer][y Integer])

(define Football%
  (class* object% (Toy<%>)

    ;; the init-fields are the values that may vary from one football to
    ;; the next.

    ;; -> Int Int
    ; the x and y position of the center of the football
    (init-field x y)   

    ;; -> Boolean
    ; is the football selected? Default is false.
    (init-field [selected? false])

    ;; -> NonNegInt NonNegInt
    ;; if the football is selected, the position of
    ;; the last button-down event inside the football, relative to the
    ;; football's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; -> NonNegReal
    ;; the initial scale value
    (init-field [scale-margin 1])

    ;; -> NonNegReal
    ;; the constant scaling factor for reducing the image size.
    (field [sf 0.1])
    
    ; -> Image
    ; image for displaying the football
    (field [FOOTBALL-IMG (bitmap "football.png")])
    
    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Void
    ;; EFFECT: An updated football size as it should be after a tick
    ;;         and a selected football doesn't shrink.
    (define/public (after-tick)
      (cond[(not selected?)
            (set! scale-margin (max (- scale-margin sf) 0))]))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent as string kev
    ;; EFFECT: a key event has no effect on the objs of football.
    (define/public (after-key-event kev)
      empty)      
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT : If the location is in inside the football, select the football
    ;          and update the centre of the football accordingly.
    (define/public (after-button-down mx my)
      (cond[(in-football? mx my)
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))]))

    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT : If the football is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false))

    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: If the football is selected, move it so that the vector from the
    ;         center to the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (cond[selected?
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))]))

    ; to-img: -> Image
    ; GIVEN: no arguments
    ; RETURNS: a scaled football image for a non-zero scaling margin
    (define (to-img)
      (if (zero? scale-margin)
          empty-image
          (scale scale-margin FOOTBALL-IMG)))
    
    ;; add-to-scene : Scene -> Void
    ;; GIVEN: a current scene
    ;; EFFECT: adds the football widgets to the current scene.
    (define/public (add-to-scene scene)
      (place-image (to-img) x y scene))
    
    ;; in-football? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this football.
    (define (in-football? other-x other-y)
      (let
        [(half-img-width (/ (image-width  (to-img)) 2))
         (half-img-height (/ (image-height (to-img)) 2))]
        (and
         (<= (- x half-img-width) other-x (+ x half-img-width))
         (<= (- y half-img-height) other-y (+ y half-img-height)))))

    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)

    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> ListOfNonNegReal
    ;; RETURNS: the footballs present width and height
    (define/public (toy-data) scale-margin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define TEST-X 250)
(define TEST-Y 300)
(define FOOTBALL-TOY (make-football TEST-X TEST-Y))
(define FOOTBALL-IMG (bitmap "football.png"))
(define FOOTBALL-AFTER-TICKS (place-image FOOTBALL-IMG
                                          TEST-X TEST-Y EMPTY-CANVAS))
(define TEST-SCENE-2 (place-image empty-image
                                  TEST-X TEST-Y
                                  EMPTY-CANVAS))

(begin-for-test
  (check-equal? (send FOOTBALL-TOY toy-x) 250)
  (check-equal? (send FOOTBALL-TOY toy-y) 300)
  (check-equal? (send FOOTBALL-TOY toy-data) 1)
  (check-equal? (begin
                  (send FOOTBALL-TOY after-key-event OTHER-EVENT)
                  (send FOOTBALL-TOY after-button-down TEST-X TEST-Y)
                  (send FOOTBALL-TOY after-drag TEST-X TEST-Y)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-button-up TEST-X TEST-Y)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY after-tick)
                  (send FOOTBALL-TOY add-to-scene EMPTY-CANVAS))
                TEST-SCENE-2))