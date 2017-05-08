;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 04

;; Question 1:
;;
;; (screensaver-3). Your boss has decided that your screensaver needs to show
;; more stuff on the screen. In the new screensaver, there will be a list
;; of rectangles.

;; Initially, there are no rectangle. Hitting the "n" key adds a new rectangle,
;; at the center of the canvas, at rest (velocity is 0).

;; When a rectangle is selected, the arrow keys increase the velocity of the
;; rectangle in the specified direction (up, down, left, or right). Each push
;; of the arrow key increases the velocity by 2 pixels/tick.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(require "extras.rkt")
(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 world-after-mouse-event
 rect-after-mouse-event
 rect-selected?
 world-rects
 rect-after-key-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; CANVAS DIMENSIONS :

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; RECTANGLE IMAGE :

(define RECT-IMAGE (rectangle 60 50 "outline" "blue"))

;; RECTANGLE SELECTED IMAGE :

(define RECT-SELECTED-IMAGE (rectangle 60 50 "outline" "red"))

;; RECTANGLE INITIAL CENTER CO-ORDINATES :

(define RECT-X-COORD 200)
(define RECT-Y-COORD 150)

;; RECTANGLE INITIAL VELOCITY :

(define RECT-V-X-COORD 0)
(define RECT-V-Y-COORD 0)

;; CANVAS MIN & MAX CO-ORDINATES (For Smooth Bounce) :

(define X-MIN 30)
(define X-MAX 370)
(define Y-MIN 25)
(define Y-MAX 275)

;; MOUSE CIRCLE IMAGE

(define CIRCLE-IMAGE (circle 5 "outline" "red"))

;; DEFAULT MOUSE COORDINATES

(define MX-ZERO 0)
(define MY-ZERO 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RECTANGLE : 

(define-struct rect(x y vx vy selected? mx my))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Rectangle is (make-rect NonNegInt NonNegInt Integer Integer Boolean
;;                           NonNegInt NonNegInt)

;; INTERPRETATIONS :
;;  - x is the x-coordinate of the centre of the rectangle
;;  - y is the y-coordinate of the centre of the rectangle
;;  - vx is the velcoity in the x-direction
;;  - vy is the velocity in the y-direction
;;  - selected? specifies if the rectangle is selected using the mouse.
;;  - x-coordinate of the mouse pointer
;;  - y-coordinate of the mouse pointer

;; DESTRUCTOR TEMPLATE :
;;
;; rect-fn : Rectangle -> ??

;; (define (rect-fn r)
;;  (...(rect-x r)
;;      (rect-y r)
;;      (rect-vx r)
;;      (rect-vy r)
;;      (rect-selected? r)
;;      (rect-mx r)
;;      (rect-my r)))

;; LIST OF RECTANGLES :

;; A ListOfRectangles is one of -

;; - empty
;; Interpretation : a sequence of Rectangles with no elements

;; - (cons r lor)
;; Interpretation : (cons r lor) represents a sequence of Rectangle's
;;                  whose first element is r
;;                  and other elements are represented by lor.

;; lor-fn : ListOfRectangles -> ??
;; (define (lor-fn lor)
;;   (cond
;;     [(empty? lor) ...]
;;     [else (...
;;             (rect-fn (first lor))
;;             (lor-fn (rest lor)))]))

;; WORLD :

(define-struct world (rects paused?))

;; CONSTRUCTOR TEMPLATE :

;; A World is a (make-world rects Boolean)
;; Interpretation: 
;; - rects is the ListOfRectangles 
;; - paused? describes whether or not the screensaver is paused.

;; DESTRUCTOR TEMPLATE :
;; world-fn : World -> ??
;; (define (world-fn w)
;;  (... (world-rects w)
;;       (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

;; Velocities for Testing

(define TEST-RECT-V-X-COORD -12)
(define TEST-RECT-V-Y-COORD 20)

;; EXAMPLES of Rectangles for testing

(define unselected-rect-at-200-150 (make-rect
                                     RECT-X-COORD
                                     RECT-Y-COORD
                                     RECT-V-X-COORD
                                     RECT-V-Y-COORD
                                     false
                                     MX-ZERO
                                     MY-ZERO))

(define unselected-rect-at-188-170 (make-rect
                                    (+ RECT-X-COORD TEST-RECT-V-X-COORD)
                                    (+ RECT-Y-COORD TEST-RECT-V-Y-COORD)
                                    TEST-RECT-V-X-COORD
                                    TEST-RECT-V-Y-COORD
                                    false
                                    MX-ZERO MY-ZERO))

(define selected-rect-at-200-150 (make-rect
                                   RECT-X-COORD
                                   RECT-Y-COORD
                                   RECT-V-X-COORD
                                   RECT-V-Y-COORD
                                   true
                                   210 160))

(define test-lor (cons (make-rect
                        RECT-X-COORD
                        RECT-Y-COORD
                        RECT-V-X-COORD
                        RECT-V-Y-COORD
                        false
                        MX-ZERO
                        MY-ZERO)
                       '()))

;; EXAMPLES of Worlds, for Testing

(define initial-world-state
  (make-world
    empty
    true))

(define paused-world
  (make-world
    test-lor
    true))

(define unselected-unpaused-world
  (make-world
    test-lor
    false))

(define unselected-paused-world
  (make-world
    test-lor
    true))

(define selected-unpaused-world
  (make-world
    (cons selected-rect-at-200-150
    (cons unselected-rect-at-200-150 '()))
    false))

(define selected-paused-world
  (make-world
    (cons selected-rect-at-200-150
    (cons unselected-rect-at-200-150 '()))
    true))

;; EXAMPLES of KeyEvents for Testing

(define pause-key-event " ")
(define non-pause-key-event "p")

;; EXAMPLES of MouseEvents for Testing:

(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; SCREENSAVER FUNCTION.

;; screensaver : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world

(define (screensaver b)
  (big-bang initial-world
            (on-tick world-after-tick b)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; place-rects : ListOfRectangles Scene -> Scene
;; GIVEN: a ListOfRectangles and a Scene
;; RETURNS: a scene like the given one, but with the given ListOfRectangles
;;          painted on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (place-rects test-lor EMPTY-CANVAS) gives a scene with the rectangle placed
;; over the canvas at point (200,150).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for ListOfRectangles

(define (place-rects lor s)
  (cond[(empty? lor) s]
       [else (place-rects
              (rest lor)
              (place-rect (first lor) s))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

;; Scene for testing

(define test-scene (place-images (list RECT-IMAGE
                                        (text "(0,0)" 12 "blue"))
                                 (list (make-posn 200 150)
                                       (make-posn 200 150))
                                 EMPTY-CANVAS))

(begin-for-test
  (check-equal? (place-rects test-lor EMPTY-CANVAS) test-scene))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; place-rect : Rectangle Scene -> Scene
;; GIVEN: a Rectangles and a Scene
;; RETURNS: a scene like the given one, but with the given Rectangle painted
;;          on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (place-rect unselected-rect-at-200-150 EMPTY-CANVAS) gives a scene with the
;; rectangle placed over the canvas at point (200,150).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectanlge on r

(define (place-rect r s)
  (if (rect-selected? r)
      (rect-selected-scene r s)
      (rect-unselected-scene r s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

;; Scene for testing

(define test-scene-1 (place-images (list RECT-IMAGE
                                        (text "(0,0)" 12 "blue"))
                                 (list (make-posn 200 150)
                                       (make-posn 200 150))
                                 EMPTY-CANVAS))
(define test-scene-2 (place-images (list RECT-SELECTED-IMAGE
                                        (text "(0,0)" 12 "red")
                                        CIRCLE-IMAGE)
                                 (list (make-posn 200 150)
                                       (make-posn 200 150)
                                       (make-posn 210 160))
                                 EMPTY-CANVAS))

(begin-for-test
  (check-equal? (place-rect unselected-rect-at-200-150 EMPTY-CANVAS) test-scene-1)
  (check-equal? (place-rect selected-rect-at-200-150 EMPTY-CANVAS) test-scene-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-selected-scene: Rectangle Scene -> Scene
;; GIVEN: a rectangle and a scene
;; WHERE: the rectangle is selected
;; REURNS: a scene like the given one, but with the given Rectangle painted
;;         on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-selected-scene selected-rect-at-200-150 EMPTY-CANVAS) gives a scene
;; with the selected rectangle placed over the canvas at point (200,150).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectanlge on r

(define (rect-selected-scene r s)
  (place-images
       (list CIRCLE-IMAGE
             (text (make-label
                    (rect-vx r)
                    (rect-vy r))
                   12 "red")
              RECT-SELECTED-IMAGE)
       (list
             (make-posn (rect-mx r) (rect-my r))
             (make-posn (rect-x r) (rect-y r))
             (make-posn (rect-x r) (rect-y r)))     
                   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

;; Scene for testing

(define test-scene-3 (place-images (list RECT-SELECTED-IMAGE
                                        (text "(0,0)" 12 "red")
                                        CIRCLE-IMAGE)
                                 (list (make-posn 200 150)
                                       (make-posn 200 150)
                                       (make-posn 210 160))
                                 EMPTY-CANVAS))

(begin-for-test
  (check-equal? (rect-selected-scene selected-rect-at-200-150 EMPTY-CANVAS)
                test-scene-3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-unselected-scene: Rectangle Scene -> Scene
;; GIVEN: a rectangle and a scene
;; WHERE: the rectangle is unselected
;; REURNS: a scene like the given one, but with the given Rectangle painted
;;         on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-unselected-scene selected-rect-at-200-150 EMPTY-CANVAS) gives a scene
;; with the unselected rectangle placed over the canvas at point (200,150).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectanlge on r

(define (rect-unselected-scene r s)
  (place-images (list (text (make-label
                            (rect-vx r)
                            (rect-vy r))
                           12 "blue")
                     RECT-IMAGE)
               (list (make-posn (rect-x r) (rect-y r))
                     (make-posn (rect-x r) (rect-y r)))      
                   s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

;; Scene for testing

(define test-scene-4 (place-images (list RECT-IMAGE
                                        (text "(0,0)" 12 "blue"))
                                 (list (make-posn 200 150)
                                       (make-posn 200 150))
                                 EMPTY-CANVAS))

(begin-for-test
  (check-equal? (rect-unselected-scene unselected-rect-at-200-150 EMPTY-CANVAS)
                test-scene-4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; make-label : Integer Integer -> String
;; GIVEN: two input integers describing velocity in x and y directions
;; RETURNS: a string formatted as "(Integer 1,Integer 2)" which describes
;;          the current velocity of the rectangle.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (make-label -12 20)
;; "(-12,20)"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (make-label vx vy)
  (string-append
   (string-append "(" (number->string vx) ",")
   (string-append (number->string vy) ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (make-label 23 -12) "(23,-12)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE: (world-to-scene paused-world) should return a canvas with
;; one unselected rectangle placed at (200,150).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-to-scene w)
       (place-rects
        (world-rects w)
        EMPTY-CANVAS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

;; image for paused-world

(define image-of-paused-world
  (place-rects test-lor
               EMPTY-CANVAS))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world)
    image-of-paused-world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; initial-world : Any -> World
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;EXAMPLE:

;; initial-world returns an initial world with no rectangles in the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define initial-world
  (make-world
   empty
   true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? initial-world initial-world-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-tick : World -> World
;; GIVEN: a current world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; For an initial-world with unselected-rect-at-200-150 in the list with
;; velocity as -12 and 20, (world-after-tick w) would return an unselected
;; rectangle centered at (188,170) .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-after-tick w)
    (if (world-paused? w)
        w
        (make-world
        (rects-after-tick (world-rects w))
         (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES:

(define test-lor-1 (cons (make-rect
                        RECT-X-COORD
                        RECT-Y-COORD
                        TEST-RECT-V-X-COORD
                        TEST-RECT-V-Y-COORD
                        false
                        MX-ZERO
                        MY-ZERO)
                       '()))

(define test-lor-2 (cons (make-rect
                        188
                        170
                        TEST-RECT-V-X-COORD
                        TEST-RECT-V-Y-COORD
                        false
                        MX-ZERO
                        MY-ZERO)
                       '()))

(define world-before-tick
  (make-world
   test-lor-1
   false))

(define unpaused-world
  (make-world
    test-lor-2
    false))

(begin-for-test
  (check-equal? (world-after-tick paused-world) paused-world)
  (check-equal? (world-after-tick world-before-tick) unpaused-world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rects-after-tick : ListOfRectangles -> ListOfRectangles
;; GIVEN: a ListOfRectangles
;; RETURNS: New ListOfRectangles with updated centers and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rects-after-tick test-lor) would return a rectangle centered at
;; (200,150) with zero velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template ListOfRectangles on lor

(define (rects-after-tick lor)
  (cond[(empty? lor) empty]
       [else (cons (rect-after-tick (first lor))
                   (rects-after-tick (rest lor)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-tick : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-tick rect1-at-200-100) would return a rectangle centered at
;; (188,120) with same velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-tick r)
  (if (rect-selected? r)
      r
      (cond[(outside-top-left-corner? r)
            (rect-after-top-left-boundary r)]
           [(outside-bottom-left-corner? r)
            (rect-after-bottom-left-boundary r)]
           [(outside-top-right-corner? r)
            (rect-after-top-right-boundary r)]
           [(outside-bottom-right-corner? r)
            (rect-after-bottom-right-boundary r)]
           [(outside-horizontal-min-boundary? r)
            (rect-after-left-boundary r)]
           [(outside-horizontal-max-boundary? r)
            (rect-after-right-boundary r)]
           [(outside-vertical-min-boundary? r)
            (rect-after-top-boundary r)]
           [(outside-vertical-max-boundary? r)
            (rect-after-bottom-boundary r)]
           [else (new-rectangle
                  (rect-x r)
                  (rect-y r)
                  (rect-vx r)
                  (rect-vy r))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES:

;; Rectangles for Testing

(define rect-1 (make-rect 32 26 -12 -20 false 0 0))
(define rect-2 (make-rect 32 273 -12 20 false 0 0))
(define rect-3 (make-rect 367 27 12 -20 false 0 0))
(define rect-4 (make-rect 368 268 12 20 false 0 0))
(define rect-5 (make-rect 31 40 -12 20 false 0 0))
(define rect-6 (make-rect 369 40 12 20 false 0 0))
(define rect-7 (make-rect 35 28 12 -20 false 0 0))
(define rect-8 (make-rect 35 272 12 20 false 0 0))
(define rect-9 (make-rect 32 26 -12 -20 true 0 0))

(define rect-11 (make-rect 30 25 12 20 false 0 0))
(define rect-22 (make-rect 30 275 12 -20 false 0 0))
(define rect-33 (make-rect 370 25 -12 20 false 0 0))
(define rect-44 (make-rect 370 275 -12 -20 false 0 0))
(define rect-55 (make-rect 30 40 12 20 false 0 0))
(define rect-66 (make-rect 370 40 -12 20 false 0 0))
(define rect-77 (make-rect 35 25 12 20 false 0 0))
(define rect-88 (make-rect 35 275 12 -20 false 0 0))
(define rect-99 (make-rect 32 26 -12 -20 true 0 0))

(begin-for-test
  (check-equal? (rect-after-tick rect-1) rect-11)
  (check-equal? (rect-after-tick rect-2) rect-22)
  (check-equal? (rect-after-tick rect-3) rect-33)
  (check-equal? (rect-after-tick rect-4) rect-44)
  (check-equal? (rect-after-tick rect-5) rect-55)
  (check-equal? (rect-after-tick rect-6) rect-66)
  (check-equal? (rect-after-tick rect-7) rect-77)
  (check-equal? (rect-after-tick rect-8) rect-88)
  (check-equal? (rect-after-tick rect-9) rect-99))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-top-left-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-top-left-boundary (make-rect 15 22 -12 -20 false 0 0)) would return a rectangle centered at
;; (30,25) with inverted velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-top-left-boundary r)
  (make-rect
             X-MIN
             Y-MIN
             (invert-velocity (rect-vx r))
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-bottom-left-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-bottom-left-boundary (make-rect 15 276 -12 20 false 0 0)) would
;; return a rectangle centered at (30,275) with inverted velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-bottom-left-boundary r)
  (make-rect
             X-MIN
             Y-MAX
             (invert-velocity (rect-vx r))
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-top-right-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-top-right-boundary (make-rect 374 22 12 -20 false 0 0)) would
;; return a rectangle centered at (370,25) with inverted velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-top-right-boundary r)
  (make-rect
             X-MAX
             Y-MIN
             (invert-velocity (rect-vx r))
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-bottom-right-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-bottom-right-boundary (make-rect 374 278 12 20 false 0 0)) would
;; return a rectangle centered at (370,275) with inverted velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-bottom-right-boundary r)
  (make-rect
             X-MAX
             Y-MAX
             (invert-velocity (rect-vx r))
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-top-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-top-boundary (make-rect 40 22 12 -20 false 0 0)) would return a
;; rectangle centered at (40,25) with inverted velocity y.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-top-boundary r)
  (make-rect
             (rect-x r)
             Y-MIN
             (rect-vx r)
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-bottom-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-bottom-boundary (make-rect 40 277 12 20 false 0 0)) would return
;; a rectangle centered at (40,275) with inverted velocity y.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-bottom-boundary r)
  (make-rect
             (rect-x r)
             Y-MAX
             (rect-vx r)
             (invert-velocity (rect-vy r))
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-left-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-left-boundary (make-rect 22 56 -12 20 false 0 0)) would return a
;; rectangle centered at (30,56) with inverted velocity x.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-left-boundary r)
  (make-rect
             X-MIN
             (rect-y r)
             (invert-velocity (rect-vx r))
             (rect-vy r)
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-right-boundary : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-right-boundary (make-rect 375 56 12 20 false 0 0)) would return a
;; rectangle centered at (370,56) with inverted velocity x.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-right-boundary r)
  (make-rect
             X-MAX
             (rect-y r)
             (invert-velocity (rect-vx r))
             (rect-vy r)
             false
             MX-ZERO
             MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; invert-velocity : Integer -> Integer
;; GIVEN: an Integer
;; RETURNS: additive inverse of the Integer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (invert-velocity 23)
;  -23

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Function Decomposition

(define (invert-velocity a)(- a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 13:

(begin-for-test
  (check-equal? (invert-velocity 12) -12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-vertical-min-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the minimum vertical boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-vetical-min-boundary? (make-rect 350 22 -12 -20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-vertical-min-boundary? r)
  (<= (+ (rect-y r) (rect-vy r)) Y-MIN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-vertical-max-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the maximum vertical boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-vetical-max-boundary? (make-rect 350 278 12 20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-vertical-max-boundary? r)
  (>= (+ (rect-y r) (rect-vy r)) Y-MAX))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-horizontal-min-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the minimum horizontal boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-horizontal-min-boundary? (make-rect 23 200 -12 20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-horizontal-min-boundary? r)
  (<= (+ (rect-x r) (rect-vx r)) X-MIN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-horizontal-max-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the maximum horizontal boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-horizontal-min-boundary? (make-rect 400 200 12 20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-horizontal-max-boundary? r)
  (>= (+ (rect-x r) (rect-vx r)) X-MAX))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-top-left-corner? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the top-left corner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-top-left-corner? (make-rect 22 22 -12 -20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-top-left-corner? r)
  (and (outside-horizontal-min-boundary? r) (outside-vertical-min-boundary? r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-top-right-corner? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the top-right corner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-top-right-corner? (make-rect 376 22 12 -20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-top-right-corner? r)
  (and (outside-horizontal-max-boundary? r) (outside-vertical-min-boundary? r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-bottom-left-corner? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the bottom-left corner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-bottom-left-corner? (make-rect 22 276 -12 20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-bottom-left-corner? r)
  (and (outside-horizontal-min-boundary? r) (outside-vertical-max-boundary? r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-bottom-right-corner? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the bottom-right corner

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-bottom-right-corner? (make-rect 378 279 12 20 false 0 0))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-bottom-right-corner? r)
  (and (outside-horizontal-max-boundary? r) (outside-vertical-max-boundary? r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (new-rectangle 200 200 -12 20)
;; (make-rect 200 200 -12 20 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Function Decomposition

(define (new-rectangle x y vx vy)(make-rect
                                  (+ x vx)
                                  (+ y vy)
                                  vx
                                  vy
                                  false
                                  MX-ZERO
                                  MY-ZERO))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (new-rectangle 200 200 -12 20)
                (make-rect 188 220 -12 20 false 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a KeyEvent
;; RETURNS: the World that should follow the given world
;; after the given KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (world-after-key-event initia-world-state " ") returns a paused world with
;; rectangles centered at (200,100) and (200,200) respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [(key=? kev "n")
     (world-after-n-press w)]
    [else (make-world
           (rects-after-key-event (world-rects w) kev)
           (world-paused? w))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define rect-10 (make-rect 200 150 -2 0 true 210 160))
(define world-1 (make-world (cons rect-10 (cons unselected-rect-at-200-150 '()))
                            false))

(begin-for-test
  (check-equal? (world-after-key-event initial-world-state "n")
                unselected-paused-world)
  (check-equal? (world-after-key-event unselected-paused-world " ")
                unselected-unpaused-world)
  (check-equal? (world-after-key-event selected-unpaused-world "left") world-1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rects-after-key-event : ListOfRectangles Keyevent -> ListOfRectangles
;; GIVEN: a ListOfRectangles and a KeyEvent
;; RETURNS: the updated ListOfRectangles based on operations associated with the
;;          given KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfRectangles on lor

(define (rects-after-key-event lor kev)
  (cond[(empty? lor) lor]
       [else (cons (rect-after-key-event (first lor) kev)
                   (rects-after-key-event (rest lor) kev))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: a Rectangle and a KeyEvent
;; RETURNS: the updated Rectangle based on operations associated with the given
;;          KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (rect-after-key-event r kev)
  (cond
    [(key=? kev "left")
     (rect-after-left-press r)]
    [(key=? kev "right")
     (rect-after-right-press r)]
    [(key=? kev "up")
     (rect-after-up-press r)]
    [(key=? kev "down")
     (rect-after-down-press r)]
    [else r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define left-key-event "left")
(define right-key-event "right")
(define up-key-event "up")
(define down-key-event "down")

(begin-for-test
  (check-equal? (rect-after-key-event selected-rect-at-200-150 left-key-event)
                (make-rect 200 150 -2 0 true 210 160))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 right-key-event)
                (make-rect 200 150 2 0 true 210 160))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 up-key-event)
                (make-rect 200 150 0 -2 true 210 160))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 down-key-event)
                (make-rect 200 150 0 2 true 210 160))
  (check-equal? (rect-after-key-event initial-world-state non-pause-key-event)
                initial-world-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-n-press : World -> World
;; GIVEN: a World
;; WHERE: key "n" has been pressed
;; RETURNS: the World that should follow the given world
;;          after the "n" press KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (world-after-n-press w)
  (make-world
   (cons (make-rect RECT-X-COORD
                    RECT-Y-COORD
                    RECT-V-X-COORD
                    RECT-V-Y-COORD
                    false
                    MX-ZERO
                    MY-ZERO)
         (world-rects w))
   (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-left-press : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: the Rectangle that should follow the given Rectangle
;;          after the "left" KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (rect-after-left-press r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r)
       (rect-y r)
       (- (rect-vx r) 2)
       (rect-vy r)
       true
       (rect-mx r) (rect-my r)) 
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define rect-after-left (make-rect 200 150 -2 0 true 210 160))

(begin-for-test
  (check-equal? (rect-after-left-press selected-rect-at-200-150)
                rect-after-left)
  (check-equal? (rect-after-left-press unselected-rect-at-200-150)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-right-press : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: the Rectangle that should follow the given Rectangle
;;          after the "right" KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (rect-after-right-press r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r)
       (rect-y r)
       (+ (rect-vx r) 2)
       (rect-vy r)
       true
       (rect-mx r) (rect-my r)) 
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define rect-after-right (make-rect 200 150 2 0 true 210 160))

(begin-for-test
  (check-equal? (rect-after-right-press selected-rect-at-200-150)
                rect-after-right)
  (check-equal? (rect-after-right-press unselected-rect-at-200-150)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-up-press : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: the Rectangle that should follow the given Rectangle
;;          after the "up" KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (rect-after-up-press r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r)
       (rect-y r)
       (rect-vx r)
       (- (rect-vy r) 2)
       true
       (rect-mx r) (rect-my r)) 
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define rect-after-up (make-rect 200 150 0 -2 true 210 160))

(begin-for-test
  (check-equal? (rect-after-up-press selected-rect-at-200-150)
                rect-after-up)
  (check-equal? (rect-after-up-press unselected-rect-at-200-150)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-down-press : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: the Rectangle that should follow the given Rectangle
;;          after the "down" KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (rect-after-down-press r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r)
       (rect-y r)
       (rect-vx r)
       (+ (rect-vy r) 2)
       true
       (rect-mx r) (rect-my r)) 
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define rect-after-down (make-rect 200 150 0 2 true 210 160))

(begin-for-test
  (check-equal? (rect-after-down-press selected-rect-at-200-150)
                rect-after-down)
  (check-equal? (rect-after-down-press unselected-rect-at-200-150)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-with-paused-toggled : World -> World
;; GIVEN: a current World
;; RETURNS: a World just like the given one, but with paused? toggled

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (world-with-paused-toggled paused-world) returns an unpaused world with rectangles centered at
;; (200,100) and (200,200) respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-with-paused-toggled w)
  (make-world
   (world-rects w)
   (not (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (world-after-mouse-event selected-unpaused-world 220 105 drag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template World on w

(define (world-after-mouse-event w mx my mev)
  (make-world
   (rects-after-mouse-event (world-rects w) mx my mev)
    (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (world-after-mouse-event selected-unpaused-world 210 165 "button-up")
                (make-world
                 (cons unselected-rect-at-200-150
                       (cons unselected-rect-at-200-150 '()))
                 false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rects-after-mouse-event :  ListOfRectangles Int Int MouseEvent -> ListOfRectangles
;; GIVEN: A ListOfRectangles, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the ListOfRectangles that should follow the given ListOfRectangles
;;          after the given mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for ListOfRectangles on lor

(define (rects-after-mouse-event lor mx my mev)
  (cond[(empty? lor) empty]
       [else (cons (rect-after-mouse-event (first lor) mx my mev)
                   (rects-after-mouse-event (rest lor) mx my mev))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define test-lor-before-mouse-event (cons (make-rect 200 150 0 0 true 210 165) '()))
(define test-lor-after-mouse-event (cons (make-rect 200 150 0 0 false 0 0) '()))
(begin-for-test
  (check-equal? (rects-after-mouse-event test-lor-before-mouse-event 210 165 "button-up")
                test-lor-after-mouse-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-mouse-event :  Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (rect-after-mouse-event selected-rect-at-200-150 220 165 button-down)
;; returns a Rectangle with rect-selected? field as true. rect-mx and rect-my
;; are both zeros still.

;; (rect-after-mouse-event selected-rect-at-200-150 220 165 drag) returns a
;; Rectangle with rect-selected? field as true and updates the centre of the
;; Rectangle using the current mouse co-ordinates (mx,my).

;; (rect-after-mouse-event selected-rect-at-200-150 220 165 button-down-event)
;; returns a Rectangle centered at the new position and updates the
;; rect-selected? field as false. rect-mx and rect-my are both zeros again.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on MouseEvent


(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r mx my)]
    [(mouse=? mev "button-up") (rect-after-button-up r mx my)]
    [else r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-mouse-event unselected-rect-at-200-150 220 165 button-down-event)
                (make-rect 200 150 0 0 true 220 165))
  (check-equal? (rect-after-mouse-event selected-rect-at-200-150 220 165 drag-event)
                (make-rect 210 155 0 0 true 220 165))
  (check-equal? (rect-after-mouse-event selected-rect-at-200-150 220 165 button-up-event)
                (make-rect 200 150 0 0 false 0 0))
  (check-equal? (rect-after-mouse-event unselected-rect-at-200-150 210 165 other-event)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button down
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-button-down unselected-rect1-at-200-100 220 205)
;  (make-rect 200 100 -12 20 true 220 205)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-button-down r mx my)
  (if (in-rect? r mx my)
      (make-rect (rect-x r) (rect-y r) (rect-vx r) (rect-vy r) true mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-button-down selected-rect-at-200-150 210 165)
                (make-rect 200 150 0 0 true 210 165))
  (check-equal? (rect-after-button-down unselected-rect-at-200-150 240 105)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-drag : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is drag
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-drag selected-rect-at-200-150 220 165)
;  (make-rect 210 100 0 0 true 220 165)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-drag r mx my)
  (if (rect-selected? r)
      (make-rect
       (+ (rect-x r) (- mx (rect-mx r)))
       (+ (rect-y r) (- my (rect-my r)))
       (rect-vx r)
       (rect-vy r)
       true
       mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-drag selected-rect-at-200-150 220 105)
                (make-rect 210 95 0 0 true 220 105))
  (check-equal? (rect-after-drag unselected-rect-at-200-150 240 105)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-up : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button up
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-button-up selected-rect-at-200-150 220 165)
;; (make-rect 220 165 0 0 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-button-up r mx my)
  (if (rect-selected? r)
      (make-rect (rect-x r) (rect-y r) (rect-vx r) (rect-vy r) false 0 0)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-button-up selected-rect-at-200-150 220 155)
                (make-rect 200 150 0 0 false 0 0))
  (check-equal? (rect-after-button-up unselected-rect-at-200-150 240 105)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; in-rect? : Rectangle Integer Integer -> Boolean
;; GIVEN: a Rectangle and x- and y- coordinates of the mouse event
;; RETURNS: true iff the given coordinate is inside the bounding box of the
;;          given Rectangle.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (in-rect? unselected-rect1-at-200-100 210 105)
;; #true

;; (in-rect? unselected-rect1-at-200-100 210 205)
;; #false

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (in-rect? r mx my)
  (and
    (<= 
      (- (rect-x r) X-MIN)
      mx
      (+ (rect-x r) X-MIN))
    (<= 
      (- (rect-y r) Y-MIN)
      my
      (+ (rect-y r) Y-MIN))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (in-rect? selected-rect-at-200-150 210 165) true)
  (check-equal? (in-rect? selected-rect-at-200-150 210 205) false))