;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")

(provide Clock%
         make-clock)

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

;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; EXAMPLES: refer test cases

(define (make-clock x y)
  (new Clock% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLOCK

;; A Clock appears at the center of the target
;; This clock displays the number of ticks since it was created.
;; They're selectable and draggable.

;; A Clock is a (new Clock% [x Integer][y Integer])

(define Clock%
  (class* object% (Toy<%>)

    ;; the init-fields are the values that may vary from one clock to
    ;; the next.

    ; -> Int
    ; the x and y position of the center of the clock
    (init-field x y)   

    ; -> Boolean
    ; is the clock selected? Default is false.
    (init-field [selected? false]) 

    ;; -> NonNegInt NonNegInt
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; -> Time
    ;; the time since the clock was created
    (init-field [t 0])

    ; -> PosInt
    ; the clock's radius
    (field [r 20])

    ; -> Image
    ; image for displaying the clock
    (field [CLOCK-IMG (circle r "outline" "red")])

    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Void
    ;; EFFECT: An updated clock time as it should be after a tick
    ;;         and a selected clock doesn't increase time.
    (define/public (after-tick)
      (cond[(not selected?) (set! t (+ t 1))]))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent as string kev
    ;; EFFECT: a key event has no effect on the objs of clock.
    (define/public (after-key-event kev)
      empty)      
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT : If the location is in inside the clock, select the clock
    ;          and update the centre of the clock accordingly.
    (define/public (after-button-down mx my)
      (cond[(in-clock? mx my)
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))]))

    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT : If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false))

    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: If the clock is selected, move it so that the vector from the
    ;         center to the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (cond[selected?
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))]))   

    ;; add-to-scene : Scene -> Void
    ;; GIVEN: a current scene
    ;; EFFECT: adds the clock widgets to the current scene.
    (define/public (add-to-scene scene)
      (place-image
       (overlay CLOCK-IMG (text (number->string t) 12 "indigo"))
       x y scene))
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    (define (in-clock? other-x other-y)
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
    ;; RETURNS: the time since the clock is created
    (define/public (toy-data) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define TEST-X 250)
(define TEST-Y 300)
(define CLOCK-TOY (make-clock TEST-X TEST-Y))
(define CLOCK-IMG (circle 20 "outline" "red"))
(define CLOCK-AFTER-TICKS (place-image
                           (overlay CLOCK-IMG
                                    (text "4" 12 "indigo"))
                           TEST-X TEST-Y EMPTY-CANVAS))
(define TEST-SCENE-2 (place-image CLOCK-AFTER-TICKS 250 300 EMPTY-CANVAS))

(begin-for-test
  (check-equal? (send CLOCK-TOY toy-x) 250)
  (check-equal? (send CLOCK-TOY toy-y) 300)
  (check-equal? (send CLOCK-TOY toy-data) 0)
  (check-equal? (begin
                  (send CLOCK-TOY after-key-event OTHER-EVENT)
                  (send CLOCK-TOY after-button-down TEST-X TEST-Y)
                  (send CLOCK-TOY after-drag TEST-X TEST-Y)
                  (send CLOCK-TOY after-tick)
                  (send CLOCK-TOY after-button-up TEST-X TEST-Y)
                  (send CLOCK-TOY after-tick)
                  (send CLOCK-TOY after-tick)
                  (send CLOCK-TOY after-tick)
                  (send CLOCK-TOY after-tick)
                  (send CLOCK-TOY add-to-scene EMPTY-CANVAS))
                TEST-SCENE-2))