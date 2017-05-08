;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")

(provide Throbber%
         make-throbber)

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

;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLES: refer test cases

(define (make-throbber x y)
  (new Throbber% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; THROBBER

;; A Throbber appear at the center of the target
;; expanding gradually until it reaches a radius of 20.
;; then it contracts to a radius of 5, and then resumes its cycle.
;; They're selectable and draggable.

;; A Throbber is a (new Throbber% [x Integer][y Integer])

(define Throbber%
  (class* object% (Toy<%>)

    ;; the init-fields are the values that may vary from one throbber to
    ;; the next.

    ; -> Int
    ; the x and y position of the center of the throbber
    (init-field x y)   

    ; -> Boolean
    ; is the throbber selected? Default is false.
    (init-field [selected? false]) 

    ;; -> NonNegInt NonNegInt
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; -> PosInt
    ;;the expanding or shrinking speed of the throbber
    (init-field [speed 3])
    
    ;; -> Boolean
    ;;true if the throbber is expanding
    (init-field [expand? true])

    ;; -> PosInt
    ;; The minimum and maximum radius of the throbber
    (field [min-r 5]) 
    (field [max-r 20])
    
    ; -> PosInt
    ; the throbber's current radius
    ; initialized to the minimum radius value
    (init-field [r min-r])
    
    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Void
    ;; EFFECT: An updated throbber size as it should be after a tick
    ;;         and a selected throbber doesn't grow.
    (define/public (after-tick)
      (cond [(not selected?)
             (cond
              [(and expand? (<= (- max-r speed) r))
               (set! r (+ r speed))
               (set! speed (- speed))
               (set! expand? (not expand?))]
              [(and (not expand?) (>= (- min-r speed) r))
               (set! r (+ r speed))
               (set! speed (- speed))
               (set! expand? (not expand?))]
              [else (set! r (+ r speed))])]))
        
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent as string kev
    ;; EFFECT: a key event has no effect on the objs of throbber.
    (define/public (after-key-event kev)
      empty)      
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT : If the location is in inside the throbber, select the square
    ;          and update the centre of the throbber accordingly.
    (define/public (after-button-down mx my)
      (cond[(in-throbber? mx my)
        (set! selected? true)
        (set! saved-mx (- mx x))
        (set! saved-my (- my y))]))

    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT : If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false))

    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: If the throbber is selected, move it so that the vector from the
    ;         center to the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (cond[selected?
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))]))   

    ; to-img: -> Image
    ; GIVEN: no arguments
    ; RETURNS: a solid green circle of the required radius at each tick 
    (define (to-img) (circle r "solid" "green"))
    
    ;; add-to-scene : Scene -> Void
    ;; GIVEN: a current scene
    ;; EFFECT: adds the throbber widgets to the current scene.
    (define/public (add-to-scene scene)
      (place-image (to-img) x y scene))
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- other-x x))
             (sqr (- other-y y)))
          (sqr r)))
    
    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> PosInt
    ;; RETURNS: the throbber's radius
    (define/public (toy-data) r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define TEST-X 250)
(define TEST-Y 300)
(define THROBBER-TOY (make-throbber TEST-X TEST-Y))
(define THROBBER-AFTER-TICKS (circle 8 "solid" "green"))
(define TEST-SCENE-1 (place-image THROBBER-AFTER-TICKS 250 300 EMPTY-CANVAS))
(begin-for-test
  (check-equal? (send THROBBER-TOY toy-x) 250)
  (check-equal? (send THROBBER-TOY toy-y) 300)
  (check-equal? (send THROBBER-TOY toy-data) 5)
  (check-equal? (begin
                  (send THROBBER-TOY after-key-event OTHER-EVENT)
                  (send THROBBER-TOY after-button-down TEST-X TEST-Y)
                  (send THROBBER-TOY after-drag TEST-X TEST-Y)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-button-up TEST-X TEST-Y)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY after-tick)
                  (send THROBBER-TOY add-to-scene EMPTY-CANVAS))
                TEST-SCENE-1))