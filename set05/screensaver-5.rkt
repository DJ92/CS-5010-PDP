;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname screensaver-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 05

;; Question 2:
;;
;; (screensaver-5). Your boss is so pleased with your work that he assigns you yet another feature.
;; Screensaver 4.0 adds the following feature:

;; When a rectangle is selected, the "d" key drops a pen down. When the pen is down, the
;; rectangle records on the screen a dot marking its center at each tick. The dot is displayed
;; as a solid black circle of radius 1.

;; When a rectangle is selected, the "u" key lifts the pen up. When the pen is up, the
;; rectangle does not leave a track on the screen.

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

(define RECT-HEIGHT 50)
(define RECT-WIDTH 60)

;; RECTANGLE IMAGE :

(define RECT-IMAGE (rectangle RECT-WIDTH RECT-HEIGHT "outline" "blue"))

;; RECTANGLE SELECTED IMAGE :

(define RECT-SELECTED-IMAGE (rectangle 60 50 "outline" "red"))

;; RECTANGLE INITIAL CENTER CO-ORDINATES :

(define RECT-X-COORD (/ CANVAS-WIDTH 2))
(define RECT-Y-COORD (/ CANVAS-HEIGHT 2))

;; RECTANGLE INITIAL VELOCITY :

(define RECT-V-X-COORD 0)
(define RECT-V-Y-COORD 0)

;; CANVAS MIN & MAX CO-ORDINATES (For Smooth Bounce) :

(define X-MIN (/ RECT-WIDTH 2))
(define X-MAX (- CANVAS-WIDTH (/ RECT-WIDTH 2)))
(define Y-MIN (/ RECT-HEIGHT 2))
(define Y-MAX (- CANVAS-HEIGHT (/ RECT-HEIGHT 2)))

;; MOUSE CIRCLE IMAGE

(define CIRCLE-IMAGE (circle 5 "outline" "red"))

;; RECTANGLE PEN IMAGE

(define PEN-IMAGE (circle 1 "solid" "black"))

;; DEFAULT MOUSE COORDINATES

(define MX-ZERO 0)
(define MY-ZERO 0)

;; VELOCITY INCREASING FACTOR

(define VELOCITY-TWO 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RECTANGLE : 

(define-struct rect(x y vx vy selected? mx my pen-down? lopdp))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Rectangle is (make-rect NonNegInt NonNegInt Integer Integer Boolean
;;                              NonNegInt NonNegInt Boolean ListOfPenDropPoints)

;; INTERPRETATIONS :
;;  - x is the x-coordinate of the centre of the rectangle
;;  - y is the y-coordinate of the centre of the rectangle
;;  - vx is the velcoity in the x-direction
;;  - vy is the velocity in the y-direction
;;  - selected? specifies if the rectangle is selected using the mouse.
;;  - x-coordinate of the mouse pointer
;;  - y-coordinate of the mouse pointer
;;  - pen-down? specifies if the rectangle's pen is down or not.
;;  - lopdp specifies the ListOfPenDropPoints of the rectangle.

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
;;      (rect-my r)
;;      (rect-lopdp r)))

;; POSN : 

;; Already defined in BSL

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Posn is (make-posn NonNegInt NonNegInt)

;; INTERPRETATIONS :
;;  - x is the x-coordinate of the point to place the pen at
;;  - y is the y-coordinate of the point  to place the pen at

;; DESTRUCTOR TEMPLATE :
;;
;; posn-fn : Posn -> ??

;; (define (posn-fn p)
;;  (...(posn-x p)
;;      (posn-y p)))

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

;; LIST OF PEN DROP POINTS :

;; A ListOfPenDropPoints is one of -

;; - empty
;; Interpretation : a sequence of PenDropPoints with no elements

;; - (cons p lopdp)
;; Interpretation : (cons p lopdp) represents a sequence of PenDropPoints
;;                  whose first element is p
;;                  and other elements are represented by lopdp.

;; lopdp-fn : ListOfPenDropPoints -> ??
;; (define (lopdp-fn lopdp)
;;   (cond
;;     [(empty? lopdp) ...]
;;     [else (...
;;             (rect-fn (first lopdp))
;;             (lopdp-fn (rest lopdp)))]))

;; WORLD :

(define-struct world (rects paused?))

;; CONSTRUCTOR TEMPLATE :

;; A World is a (make-world rects paused?)
;; Interpretation: 
;; - rects is the ListOfRectangles 
;; - paused? describes whether or not the screensaver is paused.


;; DESTRUCTOR TEMPLATE :
;; world-fn : World -> ??
;; (define (world-fn w)
;;  (... (world-rects w)
;;       (world-paused? w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                     MY-ZERO
                                     false
                                     empty))

(define unselected-rect-at-188-170 (make-rect
                                    (+ RECT-X-COORD TEST-RECT-V-X-COORD)
                                    (+ RECT-Y-COORD TEST-RECT-V-Y-COORD)
                                    TEST-RECT-V-X-COORD
                                    TEST-RECT-V-Y-COORD
                                    false
                                    MX-ZERO
                                    MY-ZERO
                                    false
                                    empty))

(define selected-rect-at-200-150 (make-rect
                                   RECT-X-COORD
                                   RECT-Y-COORD
                                   RECT-V-X-COORD
                                   RECT-V-Y-COORD
                                   true
                                   210 160
                                   false
                                   empty))

(define test-lor (cons (make-rect
                        RECT-X-COORD
                        RECT-Y-COORD
                        RECT-V-X-COORD
                        RECT-V-Y-COORD
                        false
                        MX-ZERO
                        MY-ZERO
                        false
                        empty)
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
  (big-bang (initial-world b)
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

;; STRATEGY: Use HOF foldr on lor

(define (place-rects lor s)
  (foldr
   ; Rectangle EmptyScene -> Scene
   ; GIVEN : a rectangle r and an empty scene e
   ; RETURNS : a Scene with the rectangle r placed over the Empty Scene e
   (lambda(r e)(place-rect r e))
   s
   lor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; place-rect : Rectangle Scene -> Scene
;; GIVEN: a Rectangles and a Scene
;; RETURNS: a scene like the given one, but with the given Rectangle painted
;;          on it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (place-rect unselected-rect-at-200-150 EMPTY-CANVAS) gives a scene with the
;; rectangle placed over the canvas at point (200,1500).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectanlge on r

(define (place-rect r s)
  (if (empty? (rect-lopdp r)) ; check if ListOfPenDropPoints is empty
      (if (rect-selected? r)
          (rect-selected-scene r s)
          (rect-unselected-scene r s))
      (place-pen-drops (rect-lopdp r)
                       (if (rect-selected? r)
                           (rect-selected-scene r s)
                           (rect-unselected-scene r s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (place-rect (make-rect
                             200 150
                             10 20
                             false
                             0 0
                             true
                             (list (make-posn 210 170))) EMPTY-CANVAS)
               (place-images (list RECT-IMAGE
                                        (text "(10,20)" 12 "blue")
                                        PEN-IMAGE)
                                 (list (make-posn 200 150)
                                       (make-posn 200 150)
                                       (make-posn 210 170))
                                 EMPTY-CANVAS))
  (check-equal? (place-rect (make-rect
                             200 150
                             10 20
                             true
                             210 170
                             true
                             (list (make-posn 210 170))) EMPTY-CANVAS)
               (place-images (list RECT-SELECTED-IMAGE
                                        (text "(10,20)" 12 "red")
                                        CIRCLE-IMAGE
                                        PEN-IMAGE)
                                 (list (make-posn 200 150)
                                       (make-posn 200 150)
                                       (make-posn 210 170)
                                       (make-posn 210 170))
                                 EMPTY-CANVAS)))

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

;; place-pen-drops : ListOfPenDropPoints Scene -> Scene
;; GIVEN: a ListOfPenDropPoints and a given Scene
;; RETURNS: a scene with the ListOfPenDropPoints placed over the given scene.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use HOF foldr on lopdp

(define (place-pen-drops lopdp s)
  (foldr
   ; Posn EmptyScene -> Scene 
   ; GIVEN : a Posn p and en Empty scene e
   ; RETURNS : a scene with the pen placed at posn p
   (lambda(p e)(place-pen-drop p e))
   s
   lopdp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (place-pen-drops (list (make-posn 200 150)
                                       (make-posn 200 200)) EMPTY-CANVAS)
                (place-images (list PEN-IMAGE
                                    PEN-IMAGE)
                             (list (make-posn 200 150)
                                   (make-posn 200 200))
                             EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; place-pen-drop : Posn Scene -> Scene
;; GIVEN: a Posn and a given Scene
;; RETURNS: a scene with the pen image placed at Posn over the given scene.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (place-pen-drop pdp s)
  (place-image PEN-IMAGE
               (posn-x pdp) (posn-y pdp)
               s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (place-pen-drop (make-posn 200 150) EMPTY-CANVAS)
                (place-image PEN-IMAGE
                             200 150
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

;; CONTRACT & PURPOSE STATEMENT :

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE: (world-to-scene paused-world) should return a canvas with
;; two rects, one at (200,100) and one at (200,100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for World on w

(define (world-to-scene w)
        (place-rects
         (world-rects w)
         EMPTY-CANVAS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (world-to-scene unselected-unpaused-world) test-scene-1))

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
   empty
   true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (initial-world 2) initial-world-state))

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
        (rects-after-tick (world-rects w))
         (world-paused? w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (world-after-tick initial-world-state) initial-world-state)
  (check-equal? (world-after-tick unselected-unpaused-world)
                unselected-unpaused-world))

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

;; STRATEGY: Use HOF map on lor

(define (rects-after-tick lor)
  (map
   (lambda(r)(rect-after-tick r))
   lor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rects-after-tick (list unselected-rect-at-200-150))
                (list unselected-rect-at-200-150)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-tick : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; RETURNS: new Rectangle with updated center, velocities and pen-drop points.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-tick rect1-at-200-150) would return a rectangle centered at
;; (188,120) with same velocities.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-tick r)
  (if (rect-selected? r)
      r
      (update-rect-fn r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; update-rect-fn : Rectangle -> Rectangle
;; GIVEN: a Rectangle
;; WHERE: The given rectangle is selected.
;; RETURNS: new Rectangle with updated center and velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (update-rect-fn r)
 (make-rect
  (check-x-fn (rect-x r) (rect-vx r))
  (check-y-fn (rect-y r) (rect-vy r))
  (check-vx-fn (rect-x r) (rect-vx r))
  (check-vy-fn (rect-y r) (rect-vy r))
  (rect-selected? r)
  (rect-mx r)
  (rect-my r)
  (rect-pen-down? r)
  (update-rect-pen r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-x-fn : NonNegInt Integer -> NonNegInt
;; GIVEN: a non negative x-coordinate and velocity along x as integer vx
;; WHERE: x is x-coordinate of the center point of the rectangle
;; RETURNS: an updated x-coordinate of the center point of the rectangle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on values of x and vx

(define (check-x-fn x vx)
  (cond[(outside-left-boundary? x vx) X-MIN]
       [(outside-right-boundary? x vx) X-MAX]
       [else (+ x vx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-y-fn : NonNegInt Integer -> NonNegInt
;; GIVEN: a non negative y-coordinate and velocity along y as integer vy
;; WHERE: y is y-coordinate of the center point of the rectangle
;; RETURNS: an updated y-coordinate of the center point of the rectangle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on values of y and vy

(define (check-y-fn y vy)
  (cond[(outside-top-boundary? y vy) Y-MIN]
       [(outside-bottom-boundary? y vy) Y-MAX]
       [else (+ y vy)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-vx-fn : NonNegInt Integer -> NonNegInt
;; GIVEN: a non negative x-coordinate and velocity along x as integer vx
;; WHERE: x is x-coordinate of the center point of the rectangle
;; RETURNS: an updated velocity vx along x-coordinate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on values of x and vx

(define (check-vx-fn x vx)
  (cond[(outside-left-boundary? x vx) (invert-velocity vx)]
       [(outside-right-boundary? x vx) (invert-velocity vx)]
       [else vx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-vy-fn : NonNegInt Integer -> NonNegInt
;; GIVEN: a non negative y-coordinate and velocity along y as integer vy
;; WHERE: y is y-coordinate of the center point of the rectangle
;; RETURNS: an updated velocity vy along y-coordinate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Cases on values of y and vy

(define (check-vy-fn y vy)
  (cond[(outside-top-boundary? y vy) (invert-velocity vy)]
       [(outside-bottom-boundary? y vy) (invert-velocity vy)]
       [else vy]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES:

;; Rectangles for Testing

(define rect-1 (make-rect 32 26 -12 -20 false 0 0 false empty))
(define rect-2 (make-rect 32 273 -12 20 false 0 0 false empty))
(define rect-3 (make-rect 367 27 12 -20 false 0 0 false empty))
(define rect-4 (make-rect 368 268 12 20 false 0 0 false empty))
(define rect-5 (make-rect 31 40 -12 20 false 0 0 false empty))
(define rect-6 (make-rect 369 40 12 20 false 0 0 false empty))
(define rect-7 (make-rect 35 28 12 -20 false 0 0 false empty))
(define rect-8 (make-rect 35 272 12 20 false 0 0 false empty))
(define rect-9 (make-rect 32 26 -12 -20 true 0 0 false empty))
(define rect-12 (make-rect 32 26 -12 -20 true 0 0 true empty))
(define rect-13 (make-rect 200 150 -12 -20 false 0 0 true empty))
(define rect-14 (make-rect 200 150 -12 -20 false 0 0 false empty))

(define rect-11 (make-rect 30 25 12 20 false 0 0 false empty))
(define rect-22 (make-rect 30 275 12 -20 false 0 0 false empty))
(define rect-33 (make-rect 370 25 -12 20 false 0 0 false empty))
(define rect-44 (make-rect 370 275 -12 -20 false 0 0 false empty))
(define rect-55 (make-rect 30 60 12 20 false 0 0 false empty))
(define rect-66 (make-rect 370 60 -12 20 false 0 0 false empty))
(define rect-77 (make-rect 47 25 12 20 false 0 0 false empty))
(define rect-88 (make-rect 47 275 12 -20 false 0 0 false empty))
(define rect-99 (make-rect 32 26 -12 -20 true 0 0 false empty))
(define rect-122 (make-rect 32 26 -12 -20 true 0 0 true empty))
(define rect-133 (make-rect 188 130 -12 -20 false 0 0 true (cons (make-posn 200 150) '())))
(define rect-144 (make-rect 188 130 -12 -20 false 0 0 false empty))

(begin-for-test
  (check-equal? (rect-after-tick rect-1) rect-11)
  (check-equal? (rect-after-tick rect-2) rect-22)
  (check-equal? (rect-after-tick rect-3) rect-33)
  (check-equal? (rect-after-tick rect-4) rect-44)
  (check-equal? (rect-after-tick rect-5) rect-55)
  (check-equal? (rect-after-tick rect-6) rect-66)
  (check-equal? (rect-after-tick rect-7) rect-77)
  (check-equal? (rect-after-tick rect-8) rect-88)
  (check-equal? (rect-after-tick rect-9) rect-99)
  (check-equal? (rect-after-tick rect-12) rect-122)
  (check-equal? (rect-after-tick rect-13) rect-133)
  (check-equal? (rect-after-tick rect-14) rect-144))

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

;; outside-top-boundary? : NonNegInt Integer -> Boolean
;; GIVEN: a y-coordinate of the point and the velocity vy along y
;; WHERE: point is the centre of the rectangle
;; RETURNS: true iff the y-coordinate is beyond the minimum vertical boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-top-boundary? 31 -12)
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-top-boundary? y vy)
  (<= (+ y vy) Y-MIN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-bottom-boundary? : NonNegInt Integer -> Boolean
;; GIVEN: a y-coordinate of the point and the velocity vy along y
;; WHERE: point is the centre of the rectangle
;; RETURNS: true iff the y-coordinate is beyond the maximum vertical boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-bottom-boundary? 274 12)
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-bottom-boundary? y vy)
  (>= (+ y vy) Y-MAX))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-left-boundary? : NonNegInt Integer -> Boolean
;; GIVEN: a x-coordinate of the point and the velocity vx along x
;; WHERE: point is the centre of the rectangle
;; RETURNS: true iff the x-coordinate is beyond the minimum horizontal boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-left-boundary? 31 -12)
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-left-boundary? x vx)
  (<= (+ x vx) X-MIN))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; outside-right-boundary? : NonNegInt Integer -> Boolean
;; GIVEN: a x-coordinate of the point and the velocity vx along x
;; WHERE: point is the centre of the rectangle
;; RETURNS: true iff the x-coordinate is beyond the maximum horizontal boundary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (outside-right-boundary? 372 12)
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (outside-right-boundary? x vx)
  (>= (+ x vx) X-MAX))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; new-rectangle : NonNegInt NonNegInt Integer Integer -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a new rectangle centered at (x,y), which will travel with
;;          velocity (vx, vy).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (new-rectangle 200 200 -12 20)
;; (make-rect 200 200 -12 20 false 0 0 false empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (new-rectangle x y vx vy)
      (make-rect
       x
       y
       vx
       vy
       false
       MX-ZERO
       MY-ZERO
       false
       empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (new-rectangle 200 200 -12 20)
                (make-rect 200 200 -12 20 false 0 0 false empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define rect-10 (make-rect 200 150 -2 0 true 210 160 false empty))
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

;; STRATEGY : Use Template for ListOfRectangles on lor

(define (rects-after-key-event lor kev)
  (map
   (lambda(r)(rect-after-key-event r kev))
   lor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: a Rectangle and a KeyEvent
;; RETURNS: the updated Rectangle based on operations associated with the given
;;          KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (rect-after-key-event r kev)
  (if (rect-selected? r)
      (update-rect-after-key-event r kev)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; update-rect-after-key-event : Rectangle KeyEvent -> Rectangle
;; GIVEN: a Rectangle and a KeyEvent
;; WHERE : The given rectangle is currently selected
;; RETURNS: the updated Rectangle based on operations associated with the given
;;          KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Rectangle on r

(define (update-rect-after-key-event r kev)
  (make-rect
   (rect-x r)
   (rect-y r)
   (change-in-vx-fn (rect-vx r) kev)
   (change-in-vy-fn (rect-vy r) kev)
   (rect-selected? r)
   (rect-mx r)
   (rect-my r)
   (change-rect-pen-down? (rect-pen-down? r) kev)
   (update-rect-pen r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; change-in-vx-fn : Integer KeyEvent -> Integer
;; GIVEN: a velcoity vx and a KeyEvent kev
;; WHERE : the rectangle is currently selected with velocity vx along x-axis
;; RETURNS: the updated velocity vx of the rectangle based on the operation
;;          associated with the KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (change-in-vx-fn vx kev)
  (cond [(string=? kev "left") (+ vx (- VELOCITY-TWO))]
        [(string=? kev "right") (+ vx VELOCITY-TWO)]
        [else vx]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; change-in-vy-fn : Integer KeyEvent -> Integer
;; GIVEN: a velcoity vy and a KeyEvent kev
;; WHERE : the rectangle is currently selected with velocity vy along y-axis
;; RETURNS: the updated velocity vy of the rectangle based on the operation
;;          associated with the KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (change-in-vy-fn vy kev)
  (cond [(string=? kev "up") (+ vy (- VELOCITY-TWO))]
        [(string=? kev "down") (+ vy VELOCITY-TWO)]
        [else vy]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; change-rect-pen-down? : Boolean KeyEvent -> Boolean
;; GIVEN: a pen-down? flag and KeyEvent kev
;; WHERE : the rectangle is currently selected with pen-down? flag 
;; RETURNS: the updated pen-down? flag of the rectangle based on the operation
;;          associated with the KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Cases on KeyEvent

(define (change-rect-pen-down? pen-down? kev)
  (if (string=? kev "d")
      (not pen-down?)
      pen-down?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; update-rect-pen : Rectangle -> ListOfPenDropPoints
;; GIVEN: a Rectangle
;; WHERE: pen of the rectangle is down
;; RETURNS: updated ListOfPenDropPoints for the given rectangle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for Rectangle on r

(define (update-rect-pen r)
  (if(rect-pen-down? r)
     (rect-lopdp-after-pen-down r)
     (rect-lopdp r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-lopdp-after-pen-down : Rectangle -> ListOfPenDropPoints
;; GIVEN: a Rectangle
;; WHERE: pen of the rectangle is down
;; RETURNS: updated ListOfPenDropPoints for the given rectangle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for Rectangle on r

(define (rect-lopdp-after-pen-down r)
   (cons (make-posn (rect-x r) (rect-y r)) (rect-lopdp r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(define left-key-event "left")
(define right-key-event "right")
(define up-key-event "up")
(define down-key-event "down")
(define pen-down "d")
(define pen-up "u")
(define other "f")

(begin-for-test
  (check-equal? (rect-after-key-event selected-rect-at-200-150 left-key-event)
                (make-rect 200 150 -2 0 true 210 160 false empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 right-key-event)
                (make-rect 200 150 2 0 true 210 160 false empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 up-key-event)
                (make-rect 200 150 0 -2 true 210 160 false empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 down-key-event)
                (make-rect 200 150 0 2 true 210 160 false empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 pen-down)
                (make-rect 200 150 0 0 true 210 160 true empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 pen-up)
                (make-rect 200 150 0 0 true 210 160 false empty))
  (check-equal? (rect-after-key-event selected-rect-at-200-150 other)
                selected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; world-after-n-press : World -> World
;; GIVEN: a World
;; WHERE: key "n" has been pressed
;; RETURNS: the World that should follow the given world
;;          after the "n" press KeyEvent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (world-after-n-press w)
  (make-world
   (cons (new-rectangle
          RECT-X-COORD
          RECT-Y-COORD
          RECT-V-X-COORD
          RECT-V-Y-COORD)
         (world-rects w))
   (world-paused? w)))

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

;; STRATEGY : Use HOF map on lor

(define (rects-after-mouse-event lor mx my mev)
  (map
   ; Rectangle -> Rectangle
   ; GIVEN : a Rectangle of the world
   ; RETURNS : the updated rectangle after mouse event has ocurred.
   (lambda(r)(rect-after-mouse-event r mx my mev))
   lor))

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
  (check-equal? (rect-after-mouse-event
                 unselected-rect-at-200-150
                 220 165
                 button-down-event)
                (make-rect 200 150 0 0 true 220 165 false empty))
  (check-equal? (rect-after-mouse-event
                 selected-rect-at-200-150
                 220 165
                 drag-event)
                (make-rect 210 155 0 0 true 220 165 false empty))
  (check-equal? (rect-after-mouse-event
                 selected-rect-at-200-150
                 220 165
                 button-up-event)
                (make-rect 200 150 0 0 false 0 0 false empty))
  (check-equal? (rect-after-mouse-event
                 unselected-rect-at-200-150
                 210 165
                 other-event)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button down
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-button-down unselected-rect-at-200-150 220 165)
;  (make-rect 200 150 0 0 true 220 165)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Combining Simpler Functions

(define (rect-after-button-down r mx my)
  (if (in-rect? r mx my)
      (update-rect-after-button-down r mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-down : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button down
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:

;; (rect-after-button-down unselected-rect-at-200-150 220 165)
;  (make-rect 200 150 0 0 true 220 165)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (update-rect-after-button-down r mx my)
  (make-rect
   (rect-x r)
   (rect-y r)
   (rect-vx r)
   (rect-vy r)
   true
   mx
   my
   (rect-pen-down? r)
   (rect-lopdp r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-button-down selected-rect-at-200-150 210 165)
                (make-rect 200 150 0 0 true 210 165 false empty))
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
      (update-rect-after-drag r mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; next-x : Rectangle NonNegInt -> NonNegInt
;; GIVEN: given a Rectangle with the x mouse co-ordinate.
;; WHERE: the mouse event is drag
;; RETURNS: new x-coordinate for center of the rectangle after drag event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (next-x r mx)
  (+ (rect-x r) (- mx (rect-mx r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; next-y : Rectangle NonNegInt -> NonNegInt
;; GIVEN: given a Rectangle with the y mouse co-ordinate.
;; WHERE: the mouse event is drag
;; RETURNS: new y-coordinate for center of the rectangle after drag event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (next-y r my)
  (+ (rect-y r) (- my (rect-my r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-x-with-mx-fn : Rectangle NonNegInt -> NonNegInt
;; GIVEN: given a Rectangle with the x mouse co-ordinate.
;; WHERE: the mouse event is drag
;; RETURNS: new x-coordinate for center of the rectangle after drag event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (check-x-with-mx-fn r mx)
  (cond[(outside-left-boundary?  (next-x r mx) (rect-vx r)) X-MIN]
       [(outside-right-boundary? (next-x r mx) (rect-vx r)) X-MAX]
       [else (next-x r mx)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (check-x-with-mx-fn (make-rect
                                     30 27
                                     -10 20
                                     true
                                     24 23
                                     false
                                     empty) 12) 30)
  (check-equal? (check-x-with-mx-fn (make-rect
                                     370 27
                                     10 20
                                     true
                                     267 23
                                     false
                                     empty) 382) 370))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; check-y-with-my-fn : Rectangle NonNegInt -> NonNegInt
;; GIVEN: given a Rectangle with the y mouse co-ordinate.
;; WHERE: the mouse event is drag
;; RETURNS: new y-coordinate for center of the rectangle after drag event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (check-y-with-my-fn r my)
  (cond[(outside-top-boundary?  (next-y r my) (rect-vy r)) Y-MIN]
       [(outside-bottom-boundary? (next-y r my) (rect-vy r)) Y-MAX]
       [else (next-y r my)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (check-y-with-my-fn (make-rect
                                     35 27
                                     10 -20
                                     true
                                     24 23
                                     false
                                     empty) 12) 25)
  (check-equal? (check-y-with-my-fn (make-rect
                                     35 275
                                     10 20
                                     true
                                     267 273
                                     false
                                     empty) 283) 275))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; update-rect-after-drag : Rectangle NonNegInt NonNegInt -> Rectangle
;; GIVEN: given a Rectangle with the mouse co-ordinates mx and my.
;; WHERE: the mouse event is drag
;; RETURNS: Rectangle with a new center point

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template for Rectangle on r

(define (update-rect-after-drag r mx my)
  (make-rect
   (check-x-with-mx-fn r mx)
   (check-y-with-my-fn r my)
   (rect-vx r)
   (rect-vy r)
   (rect-selected? r)
   mx
   my
   (rect-pen-down? r)
   (rect-lopdp r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-drag selected-rect-at-200-150 220 105)
                (make-rect 210 95 0 0 true 220 105 false empty))
  (check-equal? (rect-after-drag unselected-rect-at-200-150 240 105)
                unselected-rect-at-200-150))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; rect-after-button-up : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button up
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (rect-after-button-up r mx my)
  (if (rect-selected? r)
      (update-rect-after-button-up r mx my)
      r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; update-rect-after-button-up : Rectangle Integer Integer -> Rectangle
;; GIVEN: given a Rectangle with the x- and y- coordinates of a mouse event
;; WHERE: the mouse event is button up
;; RETURNS: a Rectangle after the mouse event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY: Use Template Rectangle on r

(define (update-rect-after-button-up r mx my)
  (make-rect
   (rect-x r)
   (rect-y r)
   (rect-vx r)
   (rect-vy r)
   false
   MX-ZERO
   MY-ZERO
   (rect-pen-down? r)
   (rect-lopdp r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (rect-after-button-up selected-rect-at-200-150 220 155)
                (make-rect 200 150 0 0 false 0 0 false empty))
  (check-equal? (rect-after-button-up unselected-rect-at-200-150 240 105)
                unselected-rect-at-200-150))

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

;; TEST CASE:

(begin-for-test
  (check-equal? (in-rect? selected-rect-at-200-150 210 165) true)
  (check-equal? (in-rect? selected-rect-at-200-150 210 205) false))