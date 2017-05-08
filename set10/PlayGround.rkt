;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 10

;; Question 1:

;; Your boss at the toy factory has been taking PDP, and he has been persuaded
;; to buy a "framework" from WidgetWorks International. The framework was
;; delivered as a file called WidgetWorks.rkt that provides three interfaces
;; and one function.

;; We will run the objects by creating a StatefulWorld, adding our widgets to
;; it, and then calling the run method on our world. We no longer need to call
;; big-bang ourselves.

;; Following is the PlayGround class for the toys.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "extras.rkt")
(require "interfaces.rkt")
(require "Square.rkt")
(require "Throbber.rkt")
(require "Clock.rkt")
(require "Football.rkt")

(provide PlayGround%
         make-playground-state)

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

;; KEY EVENTS
(define NEW-SQUARE-KEV "s")
(define NEW-THROBBER-KEV "t")
(define NEW-CLOCK-KEV "w")
(define NEW-FOOTBALL-KEV "f")

;; MOUSE EVENTS
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             DATA DEFINITIONS                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfToy<%> is one of
;; - empty
;; - (cons Toy% ListOfToy<%>)

;; Template:
;; lot-fn : ListOfToy<%> -> ??
;; (define (lot-fn lot)
;;   (cond
;;     [(empty? lot) ...]
;;     [else (...
;;            (...(first lot))
;;            (lot-fn (rest lot)))]))

;; A Time is a NonNegative Integer

;; A Toy is an object whose class implements Toy<%>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                FUNCTIONS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-playground-state : NonNegInt NonNegInt Integer -> PlayGround
;; GIVEN: the x & y co-ordinates of the target and the speed of transition for
;;        all the toys.
;; RETURNS: a new playground with the given list of toys starting at time t.
;; EXAMPLES: refer test cases

(define (make-playground-state speed)
  (new PlayGround% [x TARGET-INITIAL-X][y TARGET-INITIAL-Y][speed speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 CLASSES                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PlayGround is a canvas displaying a list of toys including a target
;; initially appearing in the center of the canvas.

;; A PlayGround is a
;; (new PlayGround% [x NonNegInt][y NonNegInt][speed Int])

(define PlayGround%
  (class* object% (PlaygroundState<%>)

    ; target's x and y position
    ; -> Integer
    (init-field x)
    ; -> Integer
    (init-field y)
    
    ; -> Square's travelling speed
    ; -> PosInt
    (init-field speed)
    
    ; -> ListOfToys
    (init-field [objs empty])
    
    ; is the target selected? Default is false.
    (init-field [selected? false])

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; blocks's center.  Else any value.
    ; -> NonNegInt
    (init-field [saved-mx 0])
    ; -> NonNegInt
    (init-field [saved-my 0])

    ;; -> PosInt
    ;; target's radius
    (field [r 10])
    
    ;; -> Image
    ;; private data for objects of this class.
    ;; the image of the target on the canvas
    (field [TARGET-IMG (circle r "outline" "blue")])

    ;; Required Magic
    (super-new)

    ;; -> Int
    ;; RETURNS: the x position of the target
    (define/public (target-x) x)
    
    ;; -> Int
    ;; RETURNS: the y position of the target
    (define/public (target-y) y)

    ;; -> Boolean
    ;; RETURNS: true iff the target is selected
    (define/public (target-selected?) selected?)
    
    ;; -> ListOfToys<%>
    ;; RETURNS: all the list of toys
    (define/public (get-toys) objs)
    
    ;; after-tick : -> Void
    ;; EFFECT: Updates the widget to a state that follows after a tick.
    (define/public (after-tick)
        (map
          (lambda (obj) (send obj after-tick))
          objs))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent as string kev
    ;; EFFECT: an updated list of toys for the current playground,
    ;;         after adding the appropriate toy corresponding to the
    ;;         given key event.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-SQUARE-KEV)
          (set! objs (cons (make-square-toy x y speed) objs))]  
        [(key=? kev NEW-THROBBER-KEV)
         (set! objs (cons (make-throbber x y) objs))]
        [(key=? kev NEW-CLOCK-KEV)
          (set! objs (cons (make-clock x y) objs))]
        [(key=? kev NEW-FOOTBALL-KEV)
          (set! objs (cons (make-football x y) objs))]
        [else empty]))

    ; after-drag : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: If the widget is selected, move it so that the vector from the
    ;         center to the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (cond[selected?
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))])
      (map
       (lambda(obj)(send obj after-drag mx my))
       objs))
    
    ; after-button-up : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-up event
    ; EFFECT : If the widget is selected, then unselect it.
    (define/public (after-button-up mx my)
      (set! selected? false)
      (map
       (lambda(obj)(send obj after-button-up mx my))
       objs))
    
    ; after-button-down : NonNegInt NonNegInt -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT : If the location is in inside the widget, select the widget
    ;          and update the centre of the widget accordingly.
    (define/public (after-button-down mx my)
       (cond [(in-target? mx my)
             (set! selected? true)
             (set! saved-mx (- mx x))
             (set! saved-my (- my y))])
      (map
       (lambda(obj)(send obj after-button-down mx my))
       objs))

    ; in-target? : NonNegInt NonNegInt -> Boolean
    ; GIVEN: the location of the cursor
    ; RETURNS: true iff the cursor location is within the target co-ordinates.
    ; STRATEGY: Combine Simpler Functions
    (define (in-target? mx my)
      (local
        [(define (dist-between x1 y1 x2 y2)
           (sqrt (+ (sqr (- x1 x2))
                    (sqr (- y1 y2)))))]
        (<= (dist-between mx my x y) r)))

    ;; add-to-scene : Scene -> Void
    ;; GIVEN: a current scene
    ;; EFFECT: adds the playground widgets to the current scene.
    (define/public (add-to-scene scene)
      (local
        [(define toys-scene
           (foldr
            (lambda (obj s) (send obj add-to-scene s))
            scene
            objs))]
        (place-image TARGET-IMG  x y toys-scene)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define TEST-PLAYGROUND (make-playground-state 10))
(define TEST-PLAYGROUND-SCENE
  (place-image (circle 10 "outline" "blue")
               260 310
               (place-image (bitmap "football.png")
                            260 310
                            (place-image
                             (overlay (circle 20 "outline" "red")
                                                  (text "0" 12 "indigo"))
                             260 310
                             (place-image (circle 5 "solid" "green")
                                          260 310
                                          (place-image
                                           (rectangle 40 40 "outline" "blue")
                                           270 310
                                           EMPTY-CANVAS)))
                                     )))

(begin-for-test
  (check-equal?
    (send TEST-PLAYGROUND target-x) 250)
  (check-equal?
    (send TEST-PLAYGROUND target-y)
    300)
  (check-equal?
    (send TEST-PLAYGROUND target-selected?) false)
  (check-equal?
    (send TEST-PLAYGROUND get-toys) empty)
  (check-equal?
    (begin
      (send TEST-PLAYGROUND after-key-event "s")
      (send TEST-PLAYGROUND after-tick)
      (send TEST-PLAYGROUND after-key-event "t")
      (send TEST-PLAYGROUND after-key-event "w")
      (send TEST-PLAYGROUND after-key-event "f")
      (send TEST-PLAYGROUND after-key-event " ")
      (send TEST-PLAYGROUND after-button-down 250 300)
      (send TEST-PLAYGROUND after-drag 260 310)
      (send TEST-PLAYGROUND after-button-up 250 300)
      (send TEST-PLAYGROUND add-to-scene EMPTY-CANVAS))
    TEST-PLAYGROUND-SCENE))
