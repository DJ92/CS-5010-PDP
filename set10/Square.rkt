;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")

(provide Square%
         make-square-toy)

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

;; make-square-toy : Integer Integer PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; EXAMPLES: refer test cases

(define (make-square-toy x y speed)
  (new Square% [x x][y y][speed speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SQUARE%

;; A Square appears at the center of the target and travelling at the given
;; speed.
;; It initially travels rightwards and it executes a Perfect Bounce When its
;; edge reaches the edge of the canvas. They're selectable and draggable.

;; A Square is a (new Square% [x Integer][y Integer][speed PosInt])

(define Square%
  (class* object% (Toy<%>)

    ; the init-fields are the values that may vary from one square to
    ; the next.
    
    ; -> Int Int PosInt
    ; the x and y position of the center of the square and the travelling-speed
    (init-field x y speed)

    ; -> Boolean
    ; is the square selected? Default is false.
    (init-field [selected? false]) 

    ;; -> NonNegInt NonNegInt
    ;; if the square is selected, the position of
    ;; the last button-down event inside the square, relative to the
    ;; square's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; -> PosInt
    ;; private data for objects of this class.
    ; the square's length
    (field [l 40])   

    ; -> PosInt PosInt
    ; the horizontal boundaries of the square
    (field [X-WESTEST (/ l 2)])
    (field [X-EASTEST (- CANVAS-WIDTH (/ l 2))])

    ; -> Image
    ; image for displaying the square
    (field [SQUARE-IMG (rectangle l l "outline" "blue")])
    
    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Void
    ;; GIVEN: Time
    ;; EFFECT: Updates the square position to as it should be after a tick and
    ;;         a selected square doesn't move.
    (define/public (after-tick)
      (if selected?
          this
          (unselected-square-after-tick)))

    ;; unselected-square-after-tick : -> Square
    ;; RETURNS: A square like this one or it should be after it hits the east
    ;;          or west boundary of the canvas.
    ;; NOTE:    a selected square doesn't move.
    ;; STRATEGY: Cases on Square<%>
    (define (unselected-square-after-tick)
      (cond
        [(<= (+ x speed) X-WESTEST)
         (set! x X-WESTEST)
         (set! speed (- speed))]
        [(>= (+ x speed) X-EASTEST)
         (set! x X-EASTEST)
         (set! speed (- speed))]
        [else (set! x (+ x speed))]))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent as string kev
    ;; EFFECT: a key event has no effect on the objs of square.
    (define/public (after-key-event kev) empty)      
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT : If the location is in inside the square, select the square
    ;          and update the centre of the square accordingly.
    (define/public (after-button-down mx my)
      (cond [(in-square? mx my)
             (set! selected? true)
             (set! saved-mx (- mx x))
             (set! saved-my (- my y))]))

    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT : If the square is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false))   

    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: If the square is selected, move it so that the vector from the
    ;         center to the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (cond[selected?
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))]))  

    ;; add-to-scene : Scene -> Void
    ;; GIVEN: a current scene
    ;; EFFECT: adds the square widgets to the current scene.
    (define/public (add-to-scene scene)
      (place-image SQUARE-IMG x y scene))

    ;; in-square? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this square.
    (define (in-square? other-x other-y)
      (and
       (<= (abs (- x other-x)) (/ l 2))
       (<= (abs (- y other-y)) (/ l 2))))

    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> PosInt
    ;; RETURNS: the square's travelling speed
    (define/public (toy-data) speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define TEST-X 420)
(define TEST-Y 300)
(define SQUARE-TOY (make-square-toy TEST-X TEST-Y 10))
(define SQUARE-TOY-EAST (make-square-toy 490 TEST-Y 10))
(define SQUARE-TOY-WEST (make-square-toy 40 TEST-Y -20))
(define TEST-SQUARE (rectangle 40 40 "outline" "blue"))
(define TEST-SCENE-1 (place-image TEST-SQUARE
                                TEST-X TEST-Y
                                EMPTY-CANVAS))
(define TEST-SCENE-2 (place-image TEST-SQUARE 480 300 EMPTY-CANVAS))
(define TEST-X2 40)
(define TEST-Y2 300)
(define TEST-SCENE-11 (place-image TEST-SQUARE
                                20 300
                                EMPTY-CANVAS))

(begin-for-test
  (check-equal? (send SQUARE-TOY toy-x) 420)
  (check-equal? (send SQUARE-TOY toy-y) 300)
  (check-equal? (send SQUARE-TOY toy-data) 10)
  (check-equal? (begin
                  (send SQUARE-TOY after-key-event OTHER-EVENT)
                  (send SQUARE-TOY after-button-down TEST-X TEST-Y)
                  (send SQUARE-TOY after-drag TEST-X TEST-Y)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-button-up TEST-X TEST-Y)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY after-tick)
                  (send SQUARE-TOY add-to-scene EMPTY-CANVAS))
                TEST-SCENE-2)
  (check-equal? (begin
                  (send SQUARE-TOY-WEST after-tick)
                  (send SQUARE-TOY-WEST add-to-scene EMPTY-CANVAS))
                TEST-SCENE-11))