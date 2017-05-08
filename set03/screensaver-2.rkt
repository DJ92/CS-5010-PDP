;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 03

;; Question 2:
;;
;; (screensaver-2). Your boss has now decided to build a better screensaver.
;; This one is like the original, except for the following:

;; The rectangle is selectable and draggable. Depressing the mouse button within the rectangle
;; causes the rectangle to be "selected". When the rectangle is selected, it and its velocity
;; are displayed in red instead of blue.

;; The location where the mouse grabbed the rectangle should be indicated by an outline red
;; circle of radius 5 pixels. Simply pressing the mouse button, without moving the mouse,
;; should not cause the rectangle to move on the canvas.

;; Once the rectangle has been selected, you should be able to drag it around the Universe canvas
;; with the mouse. As you drag it, the position of the mouse within the rectangle (as indicated by
;; the red circle), should not change. When the mouse button is released, the rectangle should go
;; back to its unselected state (outline blue) in its new location.

;; All of this works whether or not the simulation is paused.
;; We refer to this behavior as "smooth dragging." We will be implementing other objects with this
;; behavior in future problem sets.

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
 world-rect1
 world-rect2
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 world-after-mouse-event
 rect-after-mouse-event
 rect-selected?
 )

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

;; RECTANGLE 1 CO-ORDINATES :

(define RECT1-X-COORD 200)
(define RECT1-Y-COORD 100)

;; RECTANGLE 1 VELOCITY :

(define RECT1-V-X-COORD -12)
(define RECT1-V-Y-COORD 20)

;; RECTANGLE 2 CO-ORDINATES :

(define RECT2-X-COORD 200)
(define RECT2-Y-COORD 200)

;; RECTANGLE 2 VELOCITY :

(define RECT2-V-X-COORD 23)
(define RECT2-V-Y-COORD -14)

;; CANVAS MIN & MAX CO-ORDINATES (For Smooth Bounce) :

(define X-MIN 30)
(define X-MAX 370)
(define Y-MIN 25)
(define Y-MAX 275)

;; MOUSE CIRCLE IMAGE

(define CIRCLE-IMAGE (circle 5 "outline" "red"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RECTANGLE : 

(define-struct rect(x y vx vy selected? mx my))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Rectangle is (make-rect NonNegInt NonNegInt Integer Integer Boolean NonNegInt NonNegInt)

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

;; WORLD :

(define-struct world (rect1 rect2 paused?))

;; CONSTRUCTOR TEMPLATE :

;; A World is a (make-world Rectangle Rectangle Boolean)
;; Interpretation: 
;; - rect1 is the template rect of rectangle 1.
;; - rect2 is the template rect of rectangle 2. 
;; - paused? describes whether or not the screensaver is paused.

;; DESTRUCTOR TEMPLATE :
;; world-fn : World -> ??
;(define (world-fn w)
;;  (... (world-rect1 w)
;;       (world-rect2 w)
;;       (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

;; EXAMPLES of Rectangles for testing

(define unselected-rect1-at-200-100 (make-rect RECT1-X-COORD RECT1-Y-COORD RECT1-V-X-COORD RECT1-V-Y-COORD false 0 0))
(define unselected-rect2-at-200-200 (make-rect RECT2-X-COORD RECT2-Y-COORD RECT2-V-X-COORD RECT2-V-Y-COORD false 0 0))

(define unselected-rect1-at-188-120 (make-rect (+ RECT1-X-COORD RECT1-V-X-COORD)
                                    (+ RECT1-Y-COORD RECT1-V-Y-COORD)
                                    RECT1-V-X-COORD
                                    RECT1-V-Y-COORD
                                    false
                                    0 0))

(define unselected-rect2-at-223-186 (make-rect (+ RECT2-X-COORD RECT2-V-X-COORD)
                                    (+ RECT2-Y-COORD RECT2-V-Y-COORD)
                                    RECT2-V-X-COORD
                                    RECT2-V-Y-COORD
                                    false
                                    0 0))

(define selected-rect1-at-200-100 (make-rect
                                   RECT1-X-COORD
                                   RECT1-Y-COORD
                                   RECT1-V-X-COORD
                                   RECT1-V-Y-COORD true 210 105))

(define selected-rect2-at-200-200 (make-rect
                                   RECT2-X-COORD
                                   RECT2-Y-COORD
                                   RECT2-V-X-COORD
                                   RECT2-V-Y-COORD true 220 205))

;; EXAMPLES of Worlds, for Testing

(define initial-world-state
  (make-world
    unselected-rect1-at-200-100
    unselected-rect2-at-200-200
    false))

(define paused-world
  (make-world
    unselected-rect1-at-200-100
    unselected-rect2-at-200-200
    true))

(define unpaused-world
  (make-world
    unselected-rect1-at-188-120
    unselected-rect2-at-223-186
    false))

(define selected-unpaused-world
  (make-world
    selected-rect1-at-200-100
    unselected-rect2-at-200-200
    false))

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
  (big-bang (initial-world b)
            (on-tick world-after-tick b)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; place-rect : Rectangle Scene -> Scene
;; GIVEN: a Rectangle and a Scene
;; RETURNS: a scene like the given one, but with the given Rectangle painted
;;          on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; place-rect rect2-at-200-200 should return a canvas with rectangle centered at (200,200)
;; on the current scene.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (place-rect r s)
  (if (rect-selected? r)
      (place-images
       (list CIRCLE-IMAGE
             (text (make-label (rect-vx r) (rect-vy r)) 12 "red")
              RECT-SELECTED-IMAGE)
       (list
             (make-posn (rect-mx r) (rect-my r))
             (make-posn (rect-x r) (rect-y r))
             (make-posn (rect-x r) (rect-y r)))     
                   s)
      (place-image (overlay/align "middle" "middle"
                                  (text (make-label (rect-vx r) (rect-vy r)) 12 "indigo")
                                  RECT-IMAGE)
                   (rect-x r) (rect-y r)
                   s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 1 & 2:

(begin-for-test
  (check-equal?
   (place-rect unselected-rect2-at-200-200 EMPTY-CANVAS)
   (place-image (overlay/align "middle" "middle"
    (text "(23,-14)" 12 "indigo")
    RECT-IMAGE)
               RECT2-X-COORD RECT2-Y-COORD
    EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
   (place-rect selected-rect2-at-200-200 EMPTY-CANVAS)
   (place-images
       (list CIRCLE-IMAGE
             (text "(23,-14)" 12 "red")
              RECT-SELECTED-IMAGE)
       (list
             (make-posn 220 205)
             (make-posn 200 200)
             (make-posn 200 200))     
                   EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; make-label : Integer Integer -> String
;; GIVEN: two input integers describing velocity in x and y directions
;; RETURNS: a string formatted as "(Integer 1,Integer 2)" which describes the current velocity of the rectangle.

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

;; TEST CASE 3:

(begin-for-test
  (check-equal? (make-label 23 -12) "(23,-12)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE: (world-to-scene paused-world) should return a canvas with
;; two rects, one at (200,100) and one at (200,100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-to-scene w)
  (place-rect (world-rect1 w)
              (place-rect
               (world-rect2 w)
               EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 4:

;; image for paused-world

(define image-of-paused-world
  (place-rect unselected-rect1-at-200-100
    (place-rect unselected-rect2-at-200-200
      EMPTY-CANVAS)))

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

;; (initial world 3) returns an initial world with two rectangles centered
;; at (200,100) and (200,200) respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (initial-world b)
  (make-world
    (make-rect RECT1-X-COORD RECT1-Y-COORD RECT1-V-X-COORD RECT1-V-Y-COORD false 0 0)
    (make-rect RECT2-X-COORD RECT2-Y-COORD RECT2-V-X-COORD RECT2-V-Y-COORD false 0 0)
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 5:

(begin-for-test
  (check-equal? (initial-world 3) initial-world-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-tick : World -> World
;; GIVEN: a current world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; For an (initial-world 3), (world-after-tick w) would return rectangles at (188,120) and (223,186)
;; after a tick has been elapsed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-after-tick w)
    (if (world-paused? w)
        w
        (make-world
        (rect-after-tick (world-rect1 w))
        (rect-after-tick (world-rect2 w))          
         (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 6,7,8,9 & 10:

(begin-for-test
  (check-equal? (world-after-tick initial-world-state) unpaused-world))

;; Check Boundary

;; world before rect1 reaches the bottom edge
(define world-before-rect1-meets-edge
  (make-world
   (make-rect 92 280 -12 20 false 0 0)
   (make-rect 361 75 -23 -14 false 0 0)
   false))

;; world after rect1 reaches the bottom edge
(define world-after-rect1-meets-edge
  (make-world
   (make-rect 80 260 -12 -20 false 0 0)
   (make-rect 338 61 -23 -14 false 0 0)
   false))

;; world before rect1 reaches the left edge
(define world-before-rect1-meets-edge-left
  (make-world
   (make-rect 20 200 -12 -20 false 0 0)
   (make-rect 384 88 23 -14 false 0 0)
   false))

;; world after rect1 reaches the left edge
(define world-after-rect1-meets-edge-left
  (make-world
   (make-rect 32 180 12 -20 false 0 0)
   (make-rect 361 74 -23 -14 false 0 0)
   false))

;; world before rect2 reaches the right edge
(define world-before-rect2-meets-edge
  (make-world
   (make-rect 92 280 -12 20 false 0 0)
   (make-rect 384 88 23 -14 false 0 0)
   false))

;; world after rect2 reaches the right edge
(define world-after-rect2-meets-edge
  (make-world
   (make-rect 80 260 -12 -20 false 0 0)
   (make-rect 361 74 -23 -14 false 0 0)
   false))

;; world before rect2 reaches the top edge
(define world-before-rect2-meets-edge-top
  (make-world
   (make-rect 20 200 -12 -20 false 0 0)
   (make-rect 269 18 23 -14 false 0 0)
   false))

;; world after rect2 reaches the top edge
(define world-after-rect2-meets-edge-top
  (make-world
   (make-rect 32 180 12 -20 false 0 0)
   (make-rect 292 32 23 14 false 0 0)
   false))

(begin-for-test
  (check-equal? (world-after-tick world-before-rect1-meets-edge)
                world-after-rect1-meets-edge))

(begin-for-test
  (check-equal? (world-after-tick world-before-rect1-meets-edge-left)
                world-after-rect1-meets-edge-left))

(begin-for-test
  (check-equal? (world-after-tick world-before-rect2-meets-edge)
                world-after-rect2-meets-edge))

(begin-for-test
  (check-equal? (world-after-tick world-before-rect2-meets-edge-top)
                world-after-rect2-meets-edge-top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-tick : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: New Rectangle with updated center and velocity

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-tick rect1-at-200-100) would return a rectangle centered at (188,120) with same velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-tick r)
  (if (rect-selected? r)
      r
  (if (out-of-scene? r)
          (cond[(outside-horizontal-boundary? r)
                (new-rectangle (rect-x r) (rect-y r) (invert-velocity (rect-vx r)) (rect-vy r))]
               [(outside-vertical-boundary? r)
                (new-rectangle (rect-x r) (rect-y r) (rect-vx r) (invert-velocity (rect-vy r)))]
               [else (new-rectangle (rect-x r) (rect-y r) (rect-vx r) (rect-vy r))])
          (new-rectangle (rect-x r) (rect-y r) (rect-vx r) (rect-vy r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 11:

(begin-for-test
  (check-equal? (rect-after-tick unselected-rect1-at-200-100) unselected-rect1-at-188-120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; out-of-scene? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is out of the defined CANVAS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (out-of-scene? rect2-at-200-200)
;; #false

;; (out-of-scene? (make-rect 400 400 -12 20)
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on Template Rectangle on r (rect-x, rect-y)

(define (out-of-scene? r)
  (cond[(<= (+ (rect-x r) (rect-vx r)) X-MIN) true]
       [(<= (+ (rect-y r) (rect-vy r)) Y-MIN) true]
       [(>= (+ (rect-x r) (rect-vx r)) X-MAX) true]
       [(>= (+ (rect-y r) (rect-vy r)) Y-MAX) true]
       [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 12:

(begin-for-test
  (check-equal? (out-of-scene? (make-rect 400 300 -12 20 false 0 0)) true))

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

(define (invert-velocity a)(* a -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 13:

(begin-for-test
  (check-equal? (invert-velocity 12) -12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-vertical-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the vertical boundaries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-vetical-boundary? (make-rect 400 400 -12 20))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-vertical-boundary? r)
  (or (<= (rect-y r) Y-MIN) (>= (rect-y r) Y-MAX)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 14:

(begin-for-test
  (check-equal? (outside-vertical-boundary? (make-rect 400 400 -12 20 false 0 0))
                true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-horizontal-boundary? : Rectangle -> Boolean
;; GIVEN: a Rectangle
;; RETURNS: true iff the rectangle is beyond the horizontal boundaries

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-horizontal-boundary? (make-rect 400 200 -12 20))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (outside-horizontal-boundary? r)
  (or (<= (rect-x r) X-MIN) (>= (rect-x r) X-MAX)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 15:

(begin-for-test
  (check-equal? (outside-horizontal-boundary? (make-rect 400 200 -12 20 false 0 0))
                true))

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

(define (new-rectangle x y vx vy)(make-rect (+ x vx) (+ y vy) vx vy false 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 16:

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

;; (world-after-key-event initia-world-state " ") returns a paused world with rectangles centered at
;; (200,100) and (200,200) respectively.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 17 & 18:

(begin-for-test
  (check-equal? (world-after-key-event initial-world-state pause-key-event) paused-world))

(begin-for-test
  (check-equal? (world-after-key-event initial-world-state non-pause-key-event) initial-world-state))

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
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 19:

(begin-for-test
  (check-equal? (world-with-paused-toggled paused-world) initial-world-state))

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
    (rect-after-mouse-event (world-rect1 w) mx my mev)
    (rect-after-mouse-event (world-rect2 w) mx my mev)
    (world-paused? w)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 20:

(define world-after-drag
  (make-world
   (make-rect 200 100 -12 20 false 0 0)
   unselected-rect2-at-200-200
   false))

(begin-for-test
  (check-equal? (world-after-mouse-event initial-world-state 205 105 drag-event)
                world-after-drag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-mouse-event :  Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (rect-after-mouse-event selected-rect1-at-200-100 220 205 button-down) returns a Rectangle
;; with rect-selected? field as true. rect-mx and rect-my are both zeros still.

;; (rect-after-mouse-event selected-rect1-at-200-100 220 205 drag) returns a Rectangle
;; with rect-selected? field as true and updates the centre of the Rectangle using the current mouse
;; co-ordinates (mx,my).

;; (rect-after-mouse-event selected-rect1-at-200-100 220 205 button-down-event) returns a Rectangle
;; centered at the new position and updates the rect-selected? field as false. rect-mx and rect-my are both zeros again.'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on MouseEvent

(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-button-down r mx my)]
    [(mouse=? mev "drag") (rect-after-drag r mx my)]
    [(mouse=? mev "button-up") (rect-after-button-up r mx my)]
    [else r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 21, 22, 23, 24:

(begin-for-test
  (check-equal? (rect-after-mouse-event unselected-rect2-at-200-200 220 205 button-down-event)
                selected-rect2-at-200-200)
  (check-equal? (rect-after-mouse-event selected-rect2-at-200-200 220 205 drag-event)
                (make-rect 200 200 23 -14 true 220 205))
  (check-equal? (rect-after-mouse-event selected-rect2-at-200-200 220 205 button-up-event)
                (make-rect 200 200 23 -14 false 0 0))
  (check-equal? (rect-after-mouse-event unselected-rect1-at-200-100 210 105 other-event)
                unselected-rect1-at-200-100))

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

;; TEST CASE 25 & 26:

(begin-for-test
  (check-equal? (rect-after-button-down unselected-rect1-at-200-100 210 105) selected-rect1-at-200-100)
  (check-equal? (rect-after-button-down unselected-rect1-at-200-100 240 105) unselected-rect1-at-200-100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-drag : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is drag
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-drag selected-rect1-at-200-100 220 105)
;  (make-rect 210 100 -12 20 true 220 205)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-drag r mx my)
  (if (rect-selected? r)
      (make-rect (+ (rect-x r) (- mx (rect-mx r))) (+ (rect-y r) (- my (rect-my r))) (rect-vx r) (rect-vy r) true mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 27 & 28:

(begin-for-test
  (check-equal? (rect-after-drag selected-rect1-at-200-100 220 105)
                (make-rect 210 100 -12 20 true 220 105))
  (check-equal? (rect-after-drag unselected-rect1-at-200-100 240 105)
                (make-rect 200 100 -12 20 false 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-up : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button up
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-button-up selected-rect1-at-200-100 220 105)
;; (make-rect 220 105 -12 20 false 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-button-up r mx my)
  (if (rect-selected? r)
      (make-rect (rect-x r) (rect-y r) (rect-vx r) (rect-vy r) false 0 0)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE 29 & 30:

(begin-for-test
  (check-equal? (rect-after-button-up selected-rect1-at-200-100 220 105) (make-rect 200 100 -12 20 false 0 0))
  (check-equal? (rect-after-button-up unselected-rect1-at-200-100 240 105) unselected-rect1-at-200-100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; in-rect? : Rectangle Integer Integer -> Boolean
;; GIVEN: a Rectangle and x- and y- coordinates of the mouse event
;; RETURNS true iff the given coordinate is inside the bounding box of the given Rectangle.

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

;; TEST CASE 31 & 32:

(begin-for-test
  (check-equal? (in-rect? unselected-rect1-at-200-100 210 105) true)
  (check-equal? (in-rect? unselected-rect1-at-200-100 210 205) false))
