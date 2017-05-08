;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 09

;; Question 1:

;; Deliever toys.rkt that shows all the toys as widgets in a World.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

(provide
 make-world
 run
 make-square-toy
 make-throbber
 make-clock
 make-football
 PlaygroundState<%>
 Toy<%>)

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

;; ANY OTHER EVENT
(define OTHER-EVENT "enter")

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

;; A PlaygroundState is a (make-playground-state ListOf(Toy) Time Speed)

;; INTERP: (make-playground-state lst t s)
;; represents a playground containing the objects(a list of Toy<%>s)
;; at time t (in ticks).
;; and the square travelling at speed s (in pixels).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 INTERFACES                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define WorldState<%>
  (interface ()

    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Every object that lives in the playground must implement the Toy<%>
;; interface.

(define Toy<%> 
  (interface (Widget<%>)  ;; this means: include all the methods in Widget<%>. 
 
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y

    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The PlayGround implements the PlaygroundState<%> interface

(define PlaygroundState<%>
  (interface (WorldState<%>) ;; include all the methods in WorldState<%>. 
    
    ;; -> Integer
    ;; RETURN: the x and y coordinates of the target
    target-x
    target-y

    ;; -> Boolean
    ;; Is the target selected?
    target-selected?

    ;; -> ListOfToy<%>
    get-toys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                FUNCTIONS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-world : PosInt -> PlaygroundState<%>
;; RETURNS: a world with a target, but no toys, and in which any
;; square toys created in the future will travel at the given speed (in
;; pixels/tick)
;; EXAMPLES: refer test cases
;; STRATEGY: combine simpler functions

(define (make-world square-speed)
  (make-playground-state
    (list (make-target))
    0
    square-speed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum PosInt -> PlaygroundState<%> 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;; creates and runs a world in which square toys travel at the given
;; speed.  Returns the final state of the world.
;; EFFECT: run an initial world at the given frame rate
;; EXAMPLES: refer test cases
;; STRATEGY: combine simpler functions

(define (run rate square-speed)
  (big-bang (make-world square-speed)
    (on-tick
      (lambda (w) (send w after-tick))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (send w after-key-event kev)))
    (on-mouse
      (lambda (w mx my mev)
        (send w after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-target : -> Target
;; RETURNS: a new target at the center of the canvas
;; EXAMPLES: refer test cases

(define (make-target)
  (new Target%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-playground-state : ListOfToy<%> NonNegInt PosInt -> PlayGround
;; GIVEN: a list of toys and the starting time
;;        and the given traveling-speed of a square
;; RETURNS: a new playground with the given list of toys starting at time t.
;; EXAMPLES: refer test cases

(define (make-playground-state objs t speed)
  (new PlayGround% [objs objs][t t][speed speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; EXAMPLES: refer test cases

(define (make-square-toy x y speed)
  (new Square% [x x][y y][SPEED speed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; EXAMPLES: refer test cases

(define (make-throbber x y)
  (new Throbber% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; EXAMPLES: refer test cases

(define (make-clock x y)
  (new Clock% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; EXAMPLES: refer test cases

(define (make-football x y)
  (new Football% [x x][y y]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  CLASSES                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0. PlayGround is a canvas displaying a list of toys including a target
;; initially appear in the center of the canvas.

;; A PlayGround is a
;; (new PlayGround% [objs ListOfToy<%>][t Time][speed PosInt])

(define PlayGround%
  (class* object% (PlaygroundState<%>)

    (init-field objs)     ;  ListOfToys
    (init-field t)        ;  Time
    (init-field speed)    ;  PosInt

    ;; Required Magic
    (super-new)

    ;; after-tick : -> PlayGround
    ;; Use HOFC map on the Toys in this PlayGround
    (define/public (after-tick)
      (make-playground-state
        (map
          (lambda (obj) (send obj after-tick))
          objs)
        (+ 1 t)
        speed))

    ;; to-scene : -> Scene
    ;; Use HOFC map on the Toys in this PlayGround
    (define/public (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        objs))

    ;; after-key-event : KeyEvent -> PlayGround
    ;; STRATEGY: Cases on kev
    ;; "s" : create a a new square-shaped toy
    ;;       travelling rightward with perfect bounce.
    ;; "t" : create a new throbber which can expand and shrink in cycle.
    ;; "w" : create a clock which displays the number of ticks
    ;;       since it was created. 
    ;; "f" : create a football which gets smaller until it reaches size 0.
    ;; other keystrokes are passed on to the objects in the world.
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-SQUARE-KEV)
         (make-playground-state
          (cons (make-square-toy (target-x) (target-y) speed) objs)
          t
          speed)]  
        [(key=? kev NEW-THROBBER-KEV)
         (make-playground-state
          (cons (make-throbber (target-x) (target-y)) objs)
          t
          speed)]
        [(key=? kev NEW-CLOCK-KEV)
         (make-playground-state
          (cons (make-clock (target-x) (target-y)) objs)
          t
          speed)]
        [(key=? kev NEW-FOOTBALL-KEV)
         (make-playground-state
          (cons (make-football (target-x) (target-y)) objs)
          t
          speed)]
        [else
         (make-playground-state
          (map
           (lambda (obj) (send obj after-key-event kev))
           objs)
          t
          speed)]))
    
    ;; world-after-mouse-event : NonNegInt NonNegInt MouseEvent -> WorldState
    ;; GIVEN: a NonNegInt mx, NonNegInt my and a String mev
    ;; WHERE: mev is the MouseEvent String
    ;; RETURNS: a WorldState after the given MouseEvent mev
    ;; STRATGY: Cases on mev
    ;; "button-down": selects a toy present on the canvas.
    ;; "drag" : drags the toy across the canvas.
    ;; "button-up" : deselects the selected toy on the canvas.
    ;; else on any other event returns the current WorldState.
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN-EVENT)
         (world-after-button-down mx my)]
        [(mouse=? mev DRAG-EVENT)
         (world-after-drag mx my)]
        [(mouse=? mev BUTTON-UP-EVENT)
         (world-after-button-up mx my)]
        [else this]))

    ;; LOCAL FUNCTIONS (not present in the interface):

    ;; world-after-button-down : NonNegInt NonNegInt -> WorldState
    ;; GIVEN: a NonNegInt mx and NonNegInt my
    ;; WHERE: MouseEvent is button down
    ;; RETURNS: a WorldState
    ;; STRATGY: Use HOF map on Toys in the Playground
    (define (world-after-button-down mx my)
      (make-playground-state
        (map
          (lambda (obj) (send obj after-button-down mx my))
          objs)
        t
        speed))

    ;; world-after-button-up : NonNegInt NonNegInt -> WorldState
    ;; GIVEN: a NonNegInt mx and NonNegInt my
    ;; WHERE: MouseEvent is button up
    ;; RETURNS: a WorldState
    ;; STRATGY: Use HOF map on Toys in the Playground
    (define (world-after-button-up mx my)
      (make-playground-state
        (map
          (lambda (obj) (send obj after-button-up mx my))
          objs)
        t
        speed))

    ;; world-after-drag : NonNegInt NonNegInt -> WorldState
    ;; GIVEN: a NonNegInt mx and NonNegInt my
    ;; WHERE: MouseEvent is drag
    ;; RETURNS: a WorldState
    ;; STRATGY: Use HOF map on Toys in the Playground
    (define (world-after-drag mx my)
      (make-playground-state
        (map
          (lambda (obj) (send obj after-drag mx my))
          objs)
        t
        speed))

    ;; target? : ListOfToy -> Target
    ;; GIVEN: a ListOfToy objs
    ;; RETURNS: the toy that denotes the target on the canvas.
    (define (target? objs)
      (filter
       (lambda (obj) (send obj target?))
       objs))

    ;; -> Int
    ;; RETURNS: the x position of the target
    (define/public (target-x)
      (send (first (target? objs)) toy-x))
    
    ;; -> Int
    ;; RETURNS: the y position of the target
    (define/public (target-y)
      (send (first (target? objs)) toy-y))

    ;; -> Boolean
    ;; RETURNS: true iff the target is selected
    (define/public (target-selected?)
      (send (first (target? objs)) toy-data))
    
    ;; -> ListOfToys<%>
    ;; RETURNS: all a list of toys including the target
    (define/public (get-toys) objs)

    ;; -> Int
    (define/public (toy-data) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We have 5 classes of Toy<%>s:
;; Target, Square, Throbber, Clock, and Football

;; 1. Target appear at the center of the canvas
;; and one canvas one has one target.
;; it is selectable and draggable.

;; A Target is a (new Target% [x Integer][y Integer])

(define Target%
  (class* object% (Toy<%>)

    ; -> Int
    ; the target initially locates at the center of the canvas
    (init-field [x TARGET-INITIAL-X])
    (init-field [y TARGET-INITIAL-Y])

    ; -> Boolean
    ; is the target selected? Default is false.
    (init-field [selected? false]) 

    ; -> NonNegInt
    ;; if the target is selected, the position of
    ;; the last button-down event inside the target, relative to the
    ;; target's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; private data for objects of this class.

    ; -> NonNegInt
    ; the target's radius
    (field [r 10])   

    ; -> Image
    ; image for displaying the target
    (field [TARGET-IMG (circle r "outline" "blue")])             

    ;; Required Magic
    (super-new)
    
    ;; after-tick : -> Target
    ;; RETURNS: A target like this one
    ;; DETAILS: a target ignores time
    (define/public (after-tick)
      this)
    
    ;; after-key-event : KeyEvent -> Target
    ;; RETURNS: A target like this one
    ;; DETAILS: a target ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : NonNegInt NonNegInt -> Target
    ; GIVEN: the location of a button-down event
    ; RETURNS: if it is in the target, then seleted it
    ; STRATEGY: Cases on whether the event is in the Target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
        (new Target%
          [x x][y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)])
        this))

    ; after-button-up : NonNegInt NonNegInt -> Target
    ; GIVEN: the location of a button-up event
    ; RETURNS: If the target is in the target, then unselect it.
    ; STRATEGY: Cases on whether the event is in the Target
    (define/public (after-button-up mx my)
      (if (in-target? mx my)
        (new Target%
          [x x][y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ; after-drag : NonNegInt NonNegInt -> Target
    ; GIVEN: the location of a drag event
    ; RETURNS: If it is selected, move it so that the vector from the center to
    ;          the drag event is equal to (mx, my)
    ; STRATEGY: Cases on whether the target is selected.
    (define/public (after-drag mx my)
      (if selected?
        (new Target%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this target painted on it.
    (define/public (add-to-scene scene)
      (place-image TARGET-IMG x y scene))
    
    ;; in-target? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this target.
    (define (in-target? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    
    ;; -> Integer
    ;; RETURNS: the x position of the target
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the target
    (define/public (toy-y) y)
    
    ;; -> Boolean
    ;; RETURNS: true iff the target is selected
    (define/public (toy-data) selected?)

    ;; -> Boolean
    ;; RETURNS: true iff this toy is a target
    (define/public (target?) true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2. a Square appear at the center of the target
;; and travelling at the given speed.
;; it initially travels rightward
;; and it executes a Perfect Bounce When its edge reaches the edge of the canvas
;; they're selectable and draggable.

;; A Square is a (new Square% [x Integer][y Integer][SPEED PosInt])

(define Square%
  (class* object% (Toy<%>)

    ; -> Int Int PosInt
    ; the x and y position of the center of the square and the travelling-speed
    (init-field x y SPEED)

    ; -> Int
    ; the travelling-speed towards left
    (field [speed-to-left (- SPEED)])

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
    ; the boundaries of the square
    (field [X-WESTEST (/ l 2)])
    (field [X-EASTEST (- CANVAS-WIDTH (/ l 2))])

    ; -> Image
    ; image for displaying the square
    (field [SQUARE-IMG (rectangle l l "outline" "blue")])             

    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Square
    ;; RETURNS: A square like this one, but as it should be after a tick
    ;;          a selected square doesn't move.
    ;; STRATEGY: Cases on selected?
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
        [(<= (+ x SPEED) X-WESTEST) (square-hit-west)]
        [(>= (+ x SPEED) X-EASTEST) (square-hit-east)]
        [else (square-no-hit)]))

    ;; square-hit-west : -> Square
    ;; RETURNS: A square after it hits the west boundary of the canvas.
    ;; NOTE:    a selected square doesn't move.
    ;; DETAILS: The square moves only horizontally between the canvas edges.
    (define (square-hit-west)
      (new Square%
           [x X-WESTEST]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [SPEED SPEED]))

    ;; square-hit-east : -> Square
    ;; RETURNS: A square after it hits the east boundary of the canvas.
    ;; NOTE:    a selected square doesn't move.
    ;; DETAILS: The square moves only horizontally between the canvas edges.
    (define (square-hit-east)
      (new Square%
          [x X-EASTEST]
          [y y]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [SPEED speed-to-left]))
    
    ;; square-no-hit : -> Square
    ;; RETURNS: A square after next move in the east of west direction.
    ;; NOTE:    a selected square doesn't move.
    ;; DETAILS: The square moves only horizontally between the canvas edges.
    (define (square-no-hit)
      (new Square%
          [x (+ x SPEED)]
          [y y]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [SPEED SPEED]))
    
    ;; after-key-event : KeyEvent -> Square
    ;; RETURNS: A world like this one, but as it should be after the
    ;;          given key event.
    ;; DETAILS: a square ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Square
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the square
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
        (new Square%
          [x x][y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)]
          [SPEED SPEED])
        this))

    ; after-button-up : Integer Integer -> Square
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the square.
    ; If the square is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-square? mx my)
        (new Square%
          [x x][y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [SPEED SPEED])
        this))   

    ; after-drag : Integer Integer -> Square
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the square is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Square%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [SPEED SPEED])
        this))   

    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this square painted
    ;; on it.
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
    (define/public (toy-data) SPEED)

    ;; -> Boolean
    ;; RETURNS: true iff this toy is a target
    (define/public (target?) false)
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3. a Throbber appear at the center of the target
;; expanding gradually until it reaches a radius of 20.
;; then it contracts to a radius of 5, and then resumes its cycle.
;; they're selectable and draggable.

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
    ; the throbber's radius
    (init-field [r min-r])

    ; -> Scene
    ; image for displaying the throbber
    (field [THROBBER-IMG (circle r "solid" "green")])

    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Throbber
    ;; RETURNS: A throbber like this one, but as it should be after a tick
    ;; a selected throbber doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (unselected-throbber-after-tick)))
    
    ;; unselected-throbber-after-tick : -> Throbber
    ;; RETURNS: A throbber like this one or it should be after it has expanded
    ;;          or converged to the maximum or minimum radii respectively.
    ;; NOTE:    a selected throbber doesn't move.
    ;; STRATEGY: Cases on Throbber<%>
    (define (unselected-throbber-after-tick)
      (cond
        [(and expand? (<= (- max-r speed) r)) (reverse-throbber)]
        [(and (not expand?) (>= (- min-r speed) r)) (reverse-throbber)]
        [else (speed-unchange-throbber)]))

    ;; reverse-throbber : -> Throbber
    ;; RETURNS : A Throbber like this one but with the expand flag toggled and
    ;;           speed reversed.
    ;; DETAILS: The throbber expands or contracts between the min and max radii
    (define (reverse-throbber)
      (new Throbber%
           [x x]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [r (+ r speed)]
           [speed (- speed)]
           [expand? (not expand?)]))

    ;; speed-unchanged-throbber : -> Throbber
    ;; RETURNS : A Throbber like this one but with no change in its speed or the
    ;;           expand flag.
    ;; DETAILS: The throbber expands or contracts between the min and max radii.
    (define (speed-unchange-throbber)
      (new Throbber%
          [x x]
          [y y]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [r (+ r speed)]
          [speed speed]
          [expand? expand?]))
    
    ;; after-key-event : KeyEvent -> Throbber
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Throbber
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
        (new Throbber%
          [x x]
          [y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)]
          [r r]
          [speed speed]
          [expand? expand?])
        this))

    ; after-button-up : Integer Integer -> Throbber
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-throbber? mx my)
        (new Throbber%
          [x x][y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [r r]
          [speed speed]
          [expand? expand?])
        this))   

    ; after-drag : Integer Integer -> Throbber
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Throbber%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [r r]
          [speed speed]
          [expand? expand?])
        this))   

    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image THROBBER-IMG x y scene))
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- x other-x))
             (sqr (- y other-y)))
          (sqr r)))

    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> PosInt
    ;; RETURNS: the throbber's radius
    (define/public (toy-data) r)

    ;; -> Boolean
    ;; RETURNS: true iff this toy is a target
    (define/public (target?) false)
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4. a Clock appear at the center of the target
;; This clock displays the number of ticks since it was created.
;; they're selectable and draggable.

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
    
    ;; after-tick : Time -> Clock
    ;; RETURNS: A clock like this one, but as it should be after a tick
    ;; a selected clock doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (unselected-clock-after-tick)))
    
    ;; unselected-clock-after-tick : -> Clock
    ;; RETURNS : A clock after tick with the current tick interval updated.
    ;; DETAILS: A clock shows the current tick since it was created.
    (define (unselected-clock-after-tick)
      (new Clock%
           [x x]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [t (+ t 1)]))
 
    ;; after-key-event : KeyEvent -> Clock
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Clock
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
        (new Clock%
          [x x]
          [y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)]
          [t t])
        this))

    ; after-button-up : Integer Integer -> Clock
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-clock? mx my)
        (new Clock%
          [x x]
          [y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [t (+ t 1)])
        this))   

    ; after-drag : Integer Integer -> Clock
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the clock is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Clock%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [t t])
        this))   

    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image
       (overlay CLOCK-IMG (text (number->string t) 12 "indigo"))
       x y scene))
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    (define (in-clock? other-x other-y)
      (<= (+ (sqr (- x other-x))
             (sqr (- y other-y)))
          (sqr r)))
    
    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> PosInt
    ;; RETURNS: the time since the clock is created
    (define/public (toy-data) t)

    ;; -> Boolean
    ;; RETURNS: true iff this toy is a target
    (define/public (target?) false)
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5. a Football appear at the center of the target
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
    ;; the initial scale x and  y values
    (init-field [scale-x 1])
    (init-field [scale-y 1])

    ;; -> NonNegReal
    ;; the scaling factors for reducing the image size.
    (field [sx 0.1])
    (field [sy 0.1])

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    ; the football's radius

    ; -> Image
    ; image for displaying the football
    (field [FOOTBALL-IMG (bitmap "football.png")])
    ; -> PosInt
    ; width of the image
    (field [w (image-width FOOTBALL-IMG)])
    ; -> PosInt
    ; height of the image
    (field [h (image-height FOOTBALL-IMG)])

    ;; Required Magic
    (super-new)
    
    ;; after-tick : Time -> Football
    ;; RETURNS: A football like this one, but as it should be after a tick
    ;; a selected football doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (unselected-football-after-tick)))

    ;; unselected-football-after-tick : -> Football
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a football ignores key events
    (define (unselected-football-after-tick)
      (new Football%
           [x x]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [scale-x (- scale-x sx)]
           [scale-y (- scale-y sy)]))
 
    ;; after-key-event : KeyEvent -> Football
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a football ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Football
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the football
    (define/public (after-button-down mx my)
      (if (in-football? mx my)
        (new Football%
          [x x]
          [y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)]
          [scale-x scale-x]
          [scale-y scale-y])
        this))

    ; after-button-up : Integer Integer -> Football
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the football.
    ; If the football is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-football? mx my)
        (new Football%
          [x x]
          [y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [scale-x (- scale-x sx)]
          [scale-y (- scale-y sy)])
        this))   

    ; after-drag : Integer Integer -> Football
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the football is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Football%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my]
          [scale-x scale-x]
          [scale-y scale-y])
        this))   

    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this football painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image (scale/xy (if (<= scale-x 0)
                                 0.01
                                 scale-x)
                             (if (<= scale-y 0)
                                 0.01
                                 scale-y) FOOTBALL-IMG) x y scene))
    
    ;; in-football? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this football.
    (define (in-football? other-x other-y)
      (and
       (<= (abs (- x other-x)) (/ w 2))
       (<= (abs (- y other-y)) (/ h 2))))

    ;; -> Integer
    ;; RETURNS: the x position of the toy
    (define/public (toy-x) x)
    
    ;; -> Integer
    ;; RETURNS: the y position of the toy
    (define/public (toy-y) y)
    
    ;; -> ListOfNonNegReal
    ;; RETURNS: the footballs present width and height
    (define/public (toy-data) (list (* scale-x w) (* scale-y h)))

    ;; -> Boolean
    ;; RETURNS: true iff this toy is a target
    (define/public (target?) false)
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)
    ;; -> NonNegReal
    (define/public (for-test:scale-x) scale-x)
    ;; -> NonNegReal
    (define/public (for-test:scale-y) scale-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests

;; Clock

;; after-tick
;; unselected & selected

(define clock-scene (place-image (overlay (text (number->string 1) 12 "indigo")
                                  (circle 20 "outline" "red"))
                                     HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
                                     EMPTY-CANVAS))
(define TEST-X-COOR 250)
(define TEST-Y-COOR 250)
(define TEST-X-AFTER-DRAG 253)
(define TEST-Y-AFTER-DRAG 253)

(begin-for-test
    (local ((define c0 (new Clock%
                             [x HALF-CANVAS-WIDTH]
                             [y HALF-CANVAS-HEIGHT]
                             [selected? false]
                             [saved-mx 0]
                             [saved-my 0]
                             [t 1]))
      (define c1 (send c0 after-tick)))
      (define c2 (new Clock%
                             [x HALF-CANVAS-WIDTH]
                             [y HALF-CANVAS-HEIGHT]
                             [selected? true]
                             [saved-mx 0]
                             [saved-my 0]
                             [t 2]))
      
      (check-equal?
       (send c1 toy-data) 2)
      (check-equal?
       (send c1 target?) false) 
      (check-equal?
       (send c2 after-tick) c2)
      (check-equal?
       (send c0 after-key-event "s") c0)
      (check-equal?
       (send c0 after-drag TEST-X-COOR TEST-Y-COOR) c0)
      (check-equal?
       (send (send c2 after-drag TEST-X-AFTER-DRAG TEST-Y-AFTER-DRAG) toy-x)
       TEST-X-AFTER-DRAG)
      (check-equal?
       (send (send c2 after-drag TEST-X-AFTER-DRAG TEST-Y-AFTER-DRAG) toy-y)
       TEST-Y-AFTER-DRAG)
      (check-equal?
       (send c2 after-button-down TEST-X-COOR TEST-Y-COOR) c2)
      (check-equal?
       (send (send c2 after-button-down
                   (send c2 toy-x)
                   (send c2 toy-y)) for-test:selected?) true)
      (check-equal?
       (send (send c2 after-button-up
                   (send c2 toy-x)
                   (send c2 toy-y)) for-test:selected?) false)
      (check-equal? (send c2 after-button-up TEST-X-COOR TEST-Y-COOR) c2)
      (check-equal? (send c0 add-to-scene EMPTY-CANVAS) clock-scene)
      (check-equal? (send c0 toy-data) 1)))

;; Football

;; after-tick
;; unselected & selected

(define football-scene (place-image (scale/xy 0.01 0.01 (bitmap "football.png"))
                                    HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
                                    EMPTY-CANVAS))
(define football-scene1 (place-image (scale/xy 0.9 0.9 (bitmap "football.png"))
                                     HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
                                     EMPTY-CANVAS))
(begin-for-test
  (local ((define r0 (new Football%
                          [x HALF-CANVAS-WIDTH]
                          [y HALF-CANVAS-HEIGHT]
                          [selected? false]
                          [saved-mx 0]
                          [saved-my 0]
                          [scale-x 1]
                          [scale-y 1]))
          (define r1 (send r0 after-tick)))
    (define r2 (new Football%
                    [x HALF-CANVAS-WIDTH]
                    [y HALF-CANVAS-HEIGHT]
                    [selected? true]
                    [saved-mx 0]
                    [saved-my 0]
                    [scale-x 1]
                    [scale-y 1]))
    (define r3 (new Football%
                    [x HALF-CANVAS-WIDTH]
                    [y HALF-CANVAS-HEIGHT]
                    [selected? false]
                    [saved-mx 0]
                    [saved-my 0]
                    [scale-x 0]
                    [scale-y 0]))
    (check-equal?
     (send r1 for-test:scale-x) 0.9)
    (check-equal?
     (send r1 for-test:scale-y) 0.9)
    (check-equal?
     (send r2 after-tick) r2)
    (check-equal?
     (send r2 target?) false)
    (check-equal?
     (send r0 after-key-event "s") r0)
    (check-equal?
     (send r0 after-drag TEST-X-COOR TEST-Y-COOR) r0)
    (check-equal?
     (send (send r2 after-drag TEST-X-AFTER-DRAG TEST-Y-AFTER-DRAG) toy-x)
     TEST-X-AFTER-DRAG)
    (check-equal?
     (send (send r2 after-drag TEST-X-AFTER-DRAG TEST-Y-AFTER-DRAG) toy-y)
     TEST-Y-AFTER-DRAG)
    (check-equal?
     (send r2 after-button-down TEST-X-COOR TEST-Y-COOR) r2)
    (check-equal?
     (send (send r2 after-button-down
                 (send r2 toy-x)
                 (send r2 toy-y)) for-test:selected?) true)
    (check-equal?
     (send (send r2 after-button-up
                 (send r2 toy-x)
                 (send r2 toy-y)) for-test:selected?) false)
    (check-equal? (send r2 after-button-up TEST-X-COOR TEST-Y-COOR) r2)
    (check-equal? (send r3 add-to-scene EMPTY-CANVAS) football-scene)
    (check-equal?
     (send r1 add-to-scene EMPTY-CANVAS) football-scene1)
    (check-equal? (send r0 toy-data) (list 55 50))))


;; Test for PlayGround
(define p0 (make-playground-state
            (list (make-target)) 0 10))
(define p1 (send p0 after-tick))
(define p2 (send p0 after-key-event "s"))
(define lot2 (send p2 get-toys))
(define p3 (send p0 after-key-event "t"))
(define lot3 (send p3 get-toys))
(define p4 (send p0 after-key-event "w"))
(define lot4 (send p4 get-toys))
(define p5 (send p0 after-key-event "f"))
(define lot5 (send p5 get-toys))
(define p6 (send p0 after-key-event "h"))
(define lot6 (send p6 get-toys))

(define tar0
 (new Target%
          [x TARGET-INITIAL-X][y TARGET-INITIAL-Y]
          [selected? false]
          [saved-mx 0]
          [saved-my 0]))

(define target-scene (place-image (circle 10 "outline" "blue")
                                 TARGET-INITIAL-X TARGET-INITIAL-Y
                                 EMPTY-CANVAS))

(define lot-scene (place-image (square 40 "outline" "blue")
                                      TARGET-INITIAL-X TARGET-INITIAL-Y
                                      (place-image
                                       (circle 10 "outline" "blue")
                                       TARGET-INITIAL-X
                                       TARGET-INITIAL-Y
                                       EMPTY-CANVAS)))

(define p2-1
  (send p0 after-mouse-event
        (+ 5 TARGET-INITIAL-X) (+ 5 TARGET-INITIAL-Y) BUTTON-DOWN-EVENT))
(define lot2-1 (send p2-1 get-toys))

(define p2-2
  (send p2 after-mouse-event
        (+ 15 TARGET-INITIAL-X) (+ 15 TARGET-INITIAL-Y) BUTTON-DOWN-EVENT))
(define lot2-2 (send p2-2 get-toys))

(define p2-3
  (send p2-1 after-mouse-event
        (+ 5 TARGET-INITIAL-X) (+ 5 TARGET-INITIAL-Y) BUTTON-UP-EVENT))
(define lot2-3 (send p2-3 get-toys))

(define p2-4
  (send p2-1 after-mouse-event
        (+ 10 TARGET-INITIAL-X) (+ 10 TARGET-INITIAL-Y) DRAG-EVENT))
(define lot2-4 (send p2-4 get-toys))

(define p2-5
  (send p2-1 after-mouse-event
        (+ 10 TARGET-INITIAL-X) (+ 10 TARGET-INITIAL-Y) OTHER-EVENT))
(define lot2-5 (send p2-5 get-toys))

(define p2-6
  (send p0 after-mouse-event
        (+ 15 TARGET-INITIAL-X) (+ 15 TARGET-INITIAL-Y) BUTTON-DOWN-EVENT))
(define lot2-6 (send p2-6 get-toys))

(define p2-7
  (send p2-1 after-mouse-event
        (+ 15 TARGET-INITIAL-X) (+ 15 TARGET-INITIAL-Y) BUTTON-UP-EVENT))
(define lot2-7 (send p2-7 get-toys))

(define p2-8
  (send p0 after-mouse-event
        (+ 15 TARGET-INITIAL-X) (+ 15 TARGET-INITIAL-Y) DRAG-EVENT))
(define lot2-8 (send p2-8 get-toys))

(begin-for-test
  (check-equal?
   (send p1 toy-data) 1)
  (check-equal?
   (send p1 target-selected?) false)
  (check-equal?
   (send (first lot2) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot2) toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send (first lot3) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot3) toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send (first lot4) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot4) toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send (first lot5) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot5) toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send (first lot6) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot6) toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send (first lot2-1) toy-data) true)
  (check-equal?
   (send (first lot2-2) toy-data) 10)
  (check-equal?
   (send (first lot2-2) for-test:selected?) true)
  (check-equal?
   (send (first lot2-3) toy-data) false)
  (check-equal?
   (send (first lot2-3) target?) true)
  (check-equal?
   (send (first lot2-4) toy-data) true)
  (check-equal?
   (send (first lot2-4) toy-x) (+ 5 TARGET-INITIAL-X))
  (check-equal?
   (send (first lot2-4) toy-y) (+ 5 TARGET-INITIAL-Y))
  (check-equal?
   (send (first lot2-5) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot2-6) toy-data) false)
  (check-equal?
   (send (first lot2-7) toy-data) true)
  (check-equal?
   (send (first lot2-8) toy-x) TARGET-INITIAL-X)
  (check-equal?
   (send (first lot2-8) toy-data) false)
  (check-equal?
   (send tar0 add-to-scene EMPTY-CANVAS) target-scene)
  (check-equal?
   (send p2 to-scene) lot-scene)
  (check-equal?
   (send (make-world 1) toy-data) 0))


;; Test for Square
(define TEST-X1 200)
(define TEST-X2 500)
(define s0 (make-square-toy TEST-X1 TARGET-INITIAL-Y 5))
(define s0-1 (make-square-toy TEST-X2 TARGET-INITIAL-Y 5))
(define s0-2 (make-square-toy 20 TARGET-INITIAL-Y -5))
(define s1 (send s0 after-tick))
(define s1-1 (send s0-1 after-tick))
(define s1-2 (send s0-2 after-tick))
(define s2 (send s0 after-key-event "s"))

(define s3-1
  (send s0 after-button-down
        (+ 5 TARGET-INITIAL-X) (+ 5 TARGET-INITIAL-Y)))
(define s3-2
  (send s0 after-button-down
        TEST-X1 TARGET-INITIAL-Y))
(define s3-3
  (send s3-2 after-drag
        (+ 5 TEST-X1) (+ 5 TARGET-INITIAL-Y)))
(define s3-4
  (send s3-3 after-button-up
        (+ 10 TARGET-INITIAL-X) (+ 10 TARGET-INITIAL-Y)))
(define s3-5
  (send s3-3 after-button-up
        (+ 10 TEST-X1) (+ 10 TARGET-INITIAL-Y)))
(define s3-6 (send s3-2 after-tick))
(define s3-7
  (send s3-1 after-drag
        (+ 5 TEST-X1) (+ 5 TARGET-INITIAL-Y)))

(define square-scene (place-image (square 40 "outline" "blue")
                                  TEST-X1 TARGET-INITIAL-Y
                                  EMPTY-CANVAS))
(define X-EASTEST 480)
(define X-WESTEST 20)
(begin-for-test
  (check-equal?
   (send s2 toy-x) TEST-X1)
  (check-equal?
   (send s2 toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send s2 toy-data) 5)
  (check-equal?
   (send s1 toy-x) (+ 5 TEST-X1))
  (check-equal?
   (send s1 toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send s1-1 toy-x) X-EASTEST)
  (check-equal?
   (send s1-1 toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send s1-2 toy-x) X-WESTEST)
  (check-equal?
   (send s0 target?) false)
  (check-equal?
   (send s3-1 for-test:selected?) false)
  (check-equal?
   (send s3-2 for-test:selected?) true)
  (check-equal?
   (send s3-3 toy-x) (+ 5 TEST-X1))
  (check-equal?
   (send s3-4 for-test:selected?) true)
  (check-equal?
   (send s3-5 for-test:selected?) false)
  (check-equal?
   (send s3-6 toy-x) TEST-X1)
  (check-equal?
   (send s3-7 for-test:selected?) false)
  (check-equal?
   (send s0 add-to-scene EMPTY-CANVAS) square-scene))

    ;; Test for Throbber
(define t0 (make-throbber TEST-X1 TARGET-INITIAL-Y))
(define t1 (send t0 after-tick))
(define t1-1 (send t1 after-tick))
(define t2 (send t0 after-key-event "s"))

(define t3-1
  (send t0 after-button-down
        (+ 5 TARGET-INITIAL-X) (+ 5 TARGET-INITIAL-Y)))
(define t3-2
  (send t0 after-button-down
        TEST-X1 TARGET-INITIAL-Y))
(define t3-3
  (send t3-2 after-drag
        (+ 5 TEST-X1) (+ 5 TARGET-INITIAL-Y)))
(define t3-4
  (send t3-3 after-button-up
        (+ 10 TARGET-INITIAL-X) (+ 10 TARGET-INITIAL-Y)))
(define t3-5
  (send t3-3 after-button-up
        (+ 5 TEST-X1) (+ 5 TARGET-INITIAL-Y)))
(define t3-6
  (send t3-2 after-tick))
(define t3-7
  (send t3-1 after-drag
        (+ 5 TEST-X1) (+ 5 TARGET-INITIAL-Y)))

(define t1-2 (send t1-1 after-tick))
(define t1-3 (send t1-2 after-tick))
(define t1-4 (send t1-3 after-tick))
(define t1-5 (send t1-4 after-tick))
(define t1-6 (send t1-5 after-tick))
(define t1-7 (send t1-6 after-tick))
(define t1-8 (send t1-7 after-tick))
(define t1-9 (send t1-8 after-tick))
(define t1-10 (send t1-9 after-tick))

(define throbber-scene (place-image (circle 5 "solid" "green")
                                    TEST-X1 TARGET-INITIAL-Y
                                    EMPTY-CANVAS))
(define MIN-R 5)
(define MAX-R 20)
(define SPEED 3)

(begin-for-test
  (check-equal?
   (send t2 toy-x) TEST-X1)
  (check-equal?
   (send t2 toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send t1 toy-x) TEST-X1)
  (check-equal?
   (send t1-1 toy-x) TEST-X1)
  (check-equal?
   (send t1-1 toy-y) TARGET-INITIAL-Y)
  (check-equal?
   (send t0 target?) false)
  (check-equal?
   (send t3-1 for-test:selected?) false)
  (check-equal?
   (send t3-2 for-test:selected?) true)
  (check-equal?
   (send t3-3 toy-x) (+ 5 TEST-X1))
  (check-equal?
   (send t3-4 for-test:selected?) true)
  (check-equal?
   (send t3-5 for-test:selected?) false)
  (check-equal?
   (send t3-6 toy-x) TEST-X1)
  (check-equal?
   (send t3-7 for-test:selected?) false)  
  (check-equal?
   (send t1-4 toy-data) MAX-R)
  (check-equal?
   (send t1-5 toy-data) (- MAX-R SPEED))
  (check-equal?
   (send t1-9 toy-data) MIN-R)
  (check-equal?
   (send t1-10 toy-data) (+ MIN-R SPEED))
    (check-equal?
   (send t2 toy-data) MIN-R)
    (check-equal?
   (send t1 toy-data) (+ MIN-R SPEED))
  (check-equal?
   (send t0 add-to-scene EMPTY-CANVAS) throbber-scene))