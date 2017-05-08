;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set: 08

;; Question 2:

;; Deliever a robot on an infinite chessboard with the specified characteristics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")
(provide
 path
eval-plan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y).
;; x represents a row number
;; y represents a column number

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; A Direction is one of
;; -- "ne"
;; -- "se"
;; -- "sw"
;; -- "nw"

;; A Plan is a ListOfMove(LOM)
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LIST OF POSITION:

; A ListOfPosition (LOP) is a list of blocks on the board such that
; it is one of
; empty         INTERP: The list is empty when there are no blocks on the board
; (cons p lop)  INTERP: The list contains blocks on the board such that the 
;                       first block is a Position and remaining blocks are
;                       represented by lop

;TEMPLATE:
;lop-fn : ListOfPosition -> ??
;(define (lop-fn lop)
;  (cond
;    [(empty? lop) empty]
;    [else (...(first lop)
;              (lop-fn (rest lop)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LIST OF MOVE
; A ListOfMove (LOM) is one of
; -- empty        INTERP: There are no moves and the robot cannot move at all
; -- (cons m lom) INTERP: The list represents a sequence of moves such that first
;                         element of the list is a Move and the remaining 
;                         elements are represents by lom

;lom-fn : ListOfMove -> ??
;(define (lom-fn lom)
;  (cond
;    [(empty? lom) empty]
;    [else (...(first lom)
;              (lom-fn (rest lom)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LIST OF PLAN:
; A ListOfPlan (LOPL) is a list containing ListOfMove and ListOfPosition
; First element of the list is always a Plan (ListOfMove)
; Second element of the list is a ListOfPosition representing the squares a 
; robot has already visited
; empty           INTERP: The list when there is no plan 
; (cons pl lopl)  INTERP: The list contains all the plans such that the first 
;                       element pl is a Plan(ListOfMove) and remaining elements
;                        are represented by lopl

;TEMPLATE:
;lopl-fn : ListOfPlan -> ??
;(define (lopl-fn lopl)
;  (cond
;    [(empty? lopl) empty]
;    [else (...(first lopl)
;              (lopl-fn (rest lopl)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LIST OF DISTANCES
; A list of distances (LOD) is represented by a ListOfNonNegReal.
; It is a list of non negative real distance values such that it is one of
; empty        INTERP: the list has no distances
; (cons d lod) INTERP: the list whose first element is a NonNegReal distance
;                      and rest elements are represented by lod

;TEMPLATE:
;lod-fn : ListOfNonNegReal -> ??
;(define (lod-fn lod)
;  (cond
;    [(empty? lod) empty]
;    [else (...(first lod)
;              (lod-fn (rest lod)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; LIST OF X
; A ListOfX (LOX) a list of X's such that
; X is a list whose
; first element is a Direction in which move is made
; and the second element is the distance to be covered if the move 
; was made in that particular direction
; it can be one of
; empty        INTERP : There is no move that gives the robot the shortest path
;                       to the target
; (cons x lox) INTERP : A sequence of X's such that the first element is X and
;                        remaining elements are represented by lox

;TEMPLATE:
;lox-fn : ListOfX-> ??
;(define (lox-fn lox)
;  (cond
;    [(empty? lox) empty]
;    [else (...(first lox)
;              (lox-fn (rest lox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; LIST OF Y

; A  ListOfY is a list f Y's such that
; Y is a list whose
; the first element of the Y is the direction of the move
; and the second and third element specifying the x and y
; position after the move, respectively.
; it can be one of
; empty        INTERP : The list is empty
; (cons y loy) INTERP : A sequence of Y's such that the first element is Y and
;                        remaining elements are represented by loy




; A MaybePlan is one of
; -- False
; -- Plan

; A MaybePosition is one of
; -- False
; -- Position

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ;; CONSTANTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define BASIC-MOVES '( ("ne" 1)
                       ("se" 1)
                       ("sw" 1)
                       ("nw" 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          ;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; block-in-line? : Position Position -> Boolean
; GIVEN: Position of the robot and Position of a block on the chessboard
; RETURNS: true iff the block is diagonally aligned with the robot
;          or the next move of the robot places it on the block
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (block-in-line? pos1 pos2)
  (or
   (= (+ (first pos1) (second pos1))
      (+ (first pos2) (second pos2)))
   (= (- (first pos1) (second pos1))
      (- (first pos2) (second pos2)))
   (and (= (first pos1) (first pos2))
        (= (second pos1) (second pos2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; robot-blocked? : Position ListOfPosition -> Boolean
; GIVEN: Position of the robot and a list of blocks
; RETURNS:True iff on the next move a robot is blocked
;         by any of the blocks on the chessboard
; STRATEGY: Use HOF ormap on blocks         
; EXAMPLES: Refer Test Cases

(define (robot-blocked? rob-pos blocks)
  (ormap
   ; Position -> Boolean
   ; Given: Position of a block on the chessboard
   ; Returns: True iff the block is diagonally aligned with the
   ;          position of a robot or
   ;          on the next move the robot lands on the block
   (lambda (block-pos) (block-in-line? rob-pos block-pos))
   blocks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-direction : Position Position -> Direction
; GIVEN: A source position and a target position
; RETURNS: Direction in which target is located w.r.t the source
; STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (get-direction pos1 pos2)
  (string-append (get-vertical-dir (second pos1) (second pos2))
                 (get-horizontal-dir (first pos1) (first pos2))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-horizontal-dir : Integer Integer -> String
; GIVEN: The row numbers in which the source and target are located
; RETURNS: A string "e" if the target is located towards the east w.r.t source
;          a string "w" if the target is located towards the west w.r.t source
; STRATEGY: Cases on Position (rows) of source and target
; EXAMPLES: Refer Test Cases

(define (get-horizontal-dir rx bx)
  (if (> (- bx rx) 0)
      "e"
      "w"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-vertical-dir : Integer Integer -> String
; GIVEN: The column numbers in which the source and target are located
; RETURNS: A string "s" if the target is located towards the south w.r.t source
;          a string "n" if the target is located towards the north w.r.t source
; STRATEGY: Cases on Position (columns) of source and target
; EXAMPLES: Refer Test Cases

(define (get-vertical-dir ry by)
  (if (> (- by ry) 0)
      "s"
      "n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; eval-plan : Position ListOfPosition Plan ->  MaybePosition
; GIVEN:
; 1. the starting position of the robot,
; 2. A list of the blocks on the board
; 3. A plan for the robot's motion
; RETURNS:
; The position of the robot at the end of executing the plan, or false
; if  the plan sends the robot to or  through any block.
; STRATEGY: Recur on each move in the Plan
; HALTING MEASURE: (length moves).Length of the ListOfMove decreases
;                  at every call
; EXAMPLES: Refer Test Cases

(define (eval-plan curr-pos blocks moves)
  (cond
    [(empty? moves) curr-pos]
    [else
     (local
       ((define new-pos (robot-pos-after-next-move curr-pos (first moves)))
        (define blocks-between-src-target
          (get-blocks-between-src-trgt curr-pos new-pos blocks (first moves))))
       (if (not (robot-blocked? new-pos blocks-between-src-target))
            (eval-plan new-pos blocks (rest moves))
            false))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-blocks-between-src-trgt :
; Position Position ListOfPosition Move -> ListOfPosition
; GIVEN: Current Position of the robot(source position)
;        Position of the robot on the next move(target position)
;        List of Blocks on the chessboard
;        A move
; RETURNS: A list of all the blocks that are between the source and the target
;          and diagonally aligned with the robot position
;          i.e all blocks in the same direction as that of the target
; STRATEGY: Use HOF filter on blocks
; EXAMPLES: Refer Test Cases

(define (get-blocks-between-src-trgt curr-pos new-pos blocks move)
  (filter
   ; Position -> Boolean
   ; Given: Position of the block from a list of positions
   ; Returns: true iff the block is in the same direction as that of the target
   (lambda (block-pos) (and (block-in-line? new-pos block-pos)
                            (block-between-src-trgt? curr-pos
                                                     block-pos
                                                     move)))
   blocks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; block-between-src-trgt? : Position Position Move -> ListOfPosition
; GIVEN: Current Position of the robot(source position)
;        Position of a block
;        A move
; RETURNS: true iff the block is between the source and the target
;          and in the same direction as that of the target
; STRATEGY: Cases on Position of the block
; EXAMPLES: Refer Test Cases

(define (block-between-src-trgt? curr-pos block-pos move)
  (cond
    [(empty? block-pos) false]
    [else (and
           (string=? (get-direction curr-pos block-pos) (first move))
           (<= 1 (get-step-count curr-pos block-pos) (second move)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-step-count : Position Position -> PosInt
; GIVEN: A source position and a target position
; RETURNS: The number of steps to take so as to reach the target from the
;          given source
; STRATEGY: Cases on Position
; EXAMPLES: Refer Test Cases

(define (get-step-count pos1 pos2)
  (if (equal? (first (get-row-and-col-steps pos1 pos2)) 0)
      (second (get-row-and-col-steps pos1 pos2))
      (first (get-row-and-col-steps pos1 pos2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-row-and-col-steps : Position Position -> Position
; GIVEN: A source position and a target position
; RETURNS: A position which tells the number of rows and colums
;          between source and target
; STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (get-row-and-col-steps pos1 pos2)
  (list (abs (- (first pos2) (first pos1)))
        (abs (- (second pos2) (second pos1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; robot-pos-after-next-move : Position Move -> Position
; GIVEN: Current position of the robot and a move
; RETUNS: New position of the robot after the new move (in the
;         specified direction and after the given number of steps)
; STRATEGY: Case on Direction 
; EXAMPLES: Refer Test Cases

(define (robot-pos-after-next-move curr-pos move)
  (cond
    [(empty? move) curr-pos]
    [(equal? (first move) "ne")
     (get-target-loc-north-east curr-pos (second move))]
    [(equal? (first move) "se")
     (get-target-loc-south-east curr-pos (second move))]
    [(equal? (first move) "nw")
     (get-target-loc-north-west curr-pos (second move))]
    [(equal? (first move) "sw")
     (get-target-loc-south-west curr-pos (second move))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-target-loc-north-east : Position PosInt -> Position
; GIVEN: Current position of the robot and the number of steps it
;        is supposed to take
; RETURNS: Position of the robot after the given number of steps in
;          the north-east direction
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (get-target-loc-north-east curr-pos step-count)
  (list (+ (first curr-pos) step-count)
        (- (second curr-pos) step-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-target-loc-south-east : Position PosInt -> Position
; GIVEN: Current position of the robot and the number of steps it
;        is supposed to take
; RETURNS: Position of the robot after the given number of steps in
;          the south-east direction
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (get-target-loc-south-east curr-pos step-count)
  (list (+ (first curr-pos) step-count)
        (+ (second curr-pos) step-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-target-loc-north-west : Position PosInt -> Position
; GIVEN: Current position of the robot and the number of steps it
;        is supposed to take
; RETURNS: Position of the robot after the given number of steps in
;          the north-west direction
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (get-target-loc-north-west curr-pos step-count) 
  (list (- (first curr-pos) step-count)
        (- (second curr-pos) step-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-target-loc-south-west : Position PosInt -> Position
; GIVEN: Current position of the robot and the number of steps it
;        is supposed to take
; RETURNS: Position of the robot after the given number of steps in
;          the south-west direction
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (get-target-loc-south-west curr-pos step-count)
  (list (- (first curr-pos) step-count)
        (+ (second curr-pos) step-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; legal-plans :
;   Position Position ListOfPlan ListOfPosition ListOfMove -> ListOfPlan
; GIVEN: A source position
;        a target position
;        A ListOfPlan
;        a list of blocks on the chessboard
;        a list of basic moves
; WHERE: The ListOfPlan is a list whose first element is a Plan and
;        second elements is a ListOfPosition representing the visited
;        squares on the chessboard
; RETURNS: A list possible plans the robot can execute to reach the target
;          along with visted squares
; STRATEGY: Use HOF map and filter on BASIC-MOVES
; EXAMPLES: Refer Test Cases

(define (legal-plans src-pos trgt-pos plan-visited blocks BASIC-MOVES)
  (map
   ; Move -> ListOfPlan
   ; Given: a move
   ; Returns: A ListOfPlan whose first element is
   ;          the move appended to the plan in consideration and
   ;          the second element is a ListOfPosition representing the
   ;          squares visited
   ; Strategy: Combine Simpler functions
   (lambda (m) (list (get-plan-with-new-move-added (first plan-visited)
                                                  (list m))
                     (get-list-of-new-visited-squares (second plan-visited)
                                                 (first plan-visited)
                                                 m
                                                 src-pos
                                                 blocks)))                  
   (filter
    ; Move -> ListOfMove
    ; Given: a move
    ; Returns: A ListOfMove containing all the legal moves that take a
    ; robot to a square that is not visited
    (lambda (legal-move) (move-visiting-new-blocks? src-pos                                                        
                                                        blocks
                                                        plan-visited
                                                        legal-move))
    (filter
   ; Move -> ListOfMove
   ; Given: a move
   ; Returns: true iff the move does not send the robot to or through a block
     (lambda (m) (legal-move? src-pos trgt-pos (first plan-visited) m blocks))
     (get-best-moves-possible src-pos trgt-pos
                              (first plan-visited)
                              blocks BASIC-MOVES)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-list-of-new-visited-squares :
; ListOfPosition Plan Move Position ListOFPosition -> ListOfPosition
; GIVEN: A plan and a move
;        A source position and a list of Blocks on the chessboard
; RETURNS: A ListOfPosition representing the new squares the robot
;          visits on every new move
; STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (get-list-of-new-visited-squares v-list plan move src-pos blocks)
  (append v-list (list(robot-pos-after-next-move
                       (eval-plan src-pos blocks plan) move))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-plan-with-new-move-added : Plan Move -> Plan
; GIVEN: A plan and a move
; RETURNS: A plan with a new move added to the list of moves in the plan
; STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (get-plan-with-new-move-added plan move)
  (append plan move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; move-visiting-new-blocks? : Position ListOfPosition ListOfPlan Move -> Boolean
; GIVEN: ;A source position and a list of Blocks on the chessboard
;        A ListOfPlan and a move
; WHERE: The ListOfPlan is a list whose first element is a Plan and the
;        second element represents a ListOfPosition representing the squares
;        on the chessboard that a robot has already visited
; RETURNS: True iff the move causes a robot to visit a new square on the
;          chessboard
; STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (move-visiting-new-blocks? src-pos blocks plan-visited legal-mv)
  (local
    ((define curr-pos (eval-plan src-pos blocks (first plan-visited)))
     (define new-pos (robot-pos-after-next-move curr-pos legal-mv)))
     (not (block-visited? new-pos (second plan-visited)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; block-visited? : Position ListOfPosition-> Boolean
; GIVEN: Position of the robot after a new move
;        and a list of squares it has visited 
; RETURNS: True iff the move causes a robot to visit a an already
;           visited square on the chessboard
; STRATEGY: Use HOF ormap on lob
; EXAMPLES: Refer Test Cases

(define (block-visited? new-pos lob)
  (ormap
   (lambda (b) (equal? new-pos b))
   lob))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-best-moves-possible:
;   Position Position Plan ListOfPosition ListOfMoves -> ListOfMoves
; GIVEN: A source and tartget Position
;        A plan
;        A list of blocks on the chessboard and a list of moves
; RETURNS: A list of moves arranged in order of increasing distances
;         i.e the first move will ensure shortest path the robot will
;         take in order to reach the target
;STRATEGY: Combine Simpler functions
; EXAMPLES: Refer Test Cases

(define (get-best-moves-possible src-pos trgt-pos plan blocks moves)
  (local
    (( define curr-pos (eval-plan src-pos blocks plan))
     (define move-distance-list
       (get-distances-by-moves curr-pos trgt-pos moves))
    (define sorted-distances (get-sorted-distances move-distance-list)))
    (get-moves-with-shortest-path curr-pos trgt-pos moves sorted-distances)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-distances-by-moves : Poistion ListOfMove -> ListOfX
; GIVEN: A source position and a list of basic moves i.e
;        moves of one step in every direction
; RETURNS: A list that whose first element is a Direction in which move was made
;          and the second element is the distance to be covered if the move 
;         was made in that particular direction
; STRATEGY: Use HOF map on moves and loy
; EXAMPLES: Refer Test Cases

(define (get-distances-by-moves curr-pos trgt-pos moves)
  (map
   ; Y -> ListOfX
   ; Given: A list Y such that
    ;       the first element of the list is the direction of the move
    ;       and the second and third element specifying the x and y
    ;       position after the move, respectively.
   ; Returns:  A list of X such that
   ;       the first element of each X is the direction of the move and
   ;       second element is the distance to be covered if the move was made 
   ;       in that particular direction
   ; Strategy: Combine simpler functions
   (lambda (new-pos) (append
                      (list (first new-pos))
                      (list (get-distance (get-coords new-pos) trgt-pos))))
   (map
    ; Move -> ListOfY
    ; Given: A move
    ; Returns: A list of Y such that
    ;          the first element of each Y is the direction of the
    ;          move and the second and third element specifying the x and y
    ;          position after the move, respectively.
    ; Strategy: Combine simpler functions
    (lambda (m) (append (list (first m))
                        (robot-pos-after-next-move curr-pos  m)))
    moves)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-coords: Y -> Position
; GIVEN: A list of Y such that
;          the first element of Y is the direction of the
;          move and the second and third element specifying the x and y
;          position after the move, respectively.
; RETURNS: A position after the given move
; STRATEGY: Combine simpler functions
; EXAMPLES: Refer Test Cases

(define (get-coords new-pos)
  (append (list (second new-pos))
          (list (third new-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-moves-with-shortest-path : ListOfX ListOfDistance -> ListOfMove
; GIVEN: A list of X such that
;       the first element of each X is the direction of the move and
;       second element is the distance to be covered if the move was made 
;       in that particular direction and
;       A list of non negative real numbers representing distances
; RETURNS: A list of moves such that the list is arranged in ascending order
;         of distances i.e the move that ensures a shortest distance to the target
;         should be executed first
; STRATEGY: Use template for lod
; EXAMPLES: Refer Test Cases

(define (get-moves-with-shortest-path curr-pos trgt-pos mv-list dist-list)
  (cond
    [(empty? dist-list) empty]
    [else (append
           (list(get-final-best-moves curr-pos
                                      trgt-pos
                                      mv-list
                                      (first dist-list)))
           (get-moves-with-shortest-path curr-pos
                                         trgt-pos
                                         mv-list
                                         (rest dist-list)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-final-best-moves : NonNegReal ListOfX -> ListOfMove
; GIVEN: A distance from source to target due to a move and
;        A list of X such that
;       the first element of each X is the direction of the move and
;       second element is the distance to be covered if the move was made 
;       in that particular direction
; RETURNS: A list of moves such that the list is arranged in ascending order
;         of distances i.e the move that ensures a shortest distance to the target
;         should be executed first
; STRATEGY: Use template for lox
; EXAMPLES: Refer Test Cases

(define (get-final-best-moves curr-pos trgt-pos mv-list dist)
  (cond
    [(empty? mv-list) empty]
    [else (local
            ((define new-pos
               (robot-pos-after-next-move curr-pos(first mv-list))))
            (if (equal? dist (get-distance new-pos trgt-pos))
                (first mv-list)
                (get-final-best-moves curr-pos
                                      trgt-pos
                                      (rest mv-list)
                                      dist)))]))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
; get-distance : Position Position -> NonNegReal
; GIVEN: A source position and target position
; RETURNS: distance of the target from the source
; STRATEGY: Combine Simpler Functions
; EXAMPLES: Refer Test Cases

(define (get-distance new-pos trgt-pos)
  (sqrt(+ (sqr (- (first new-pos) (first trgt-pos)))
          (sqr (- (second new-pos) (second trgt-pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-sorted-distances : ListOfX -> ListOfNonNegReal
; GIVEN: A list of X such that
;       the first element of each X is the direction of the move and
;       second element is the distance to be covered if the move was made 
;       in that particular direction
; RETURNS: A list of distances sorted in ascending order
; STRATEGY: Combine simpler functions
; EXAMPLES: Refer Test Cases

(define (get-sorted-distances md-list)
  (sort
   (map
   (lambda (md) (second md))
   md-list)
   <))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; legal-move? : Position Position Plan Move ListOfPosition -> Boolean
; GIVEN: A source position
;        a target position
;        a plan
;        a Move
;        a list of blocks on the chessboard
;RETURNS: True iff the given move does not cause the robot to reach or
;         jump through a block in its path
;STRATEGY: Cases on Position
;EXAMPLES: Refer Test Cases


(define (legal-move? src-pos trgt-pos plan move blocks)
  (local
    ((define curr-pos (eval-plan src-pos blocks plan))
     (define new-pos (robot-pos-after-next-move curr-pos move))
     (define blocks-between-src-trgt (get-blocks-between-src-trgt curr-pos
                                                          new-pos
                                                          blocks
                                                          move)))
  (if (not (robot-blocked? new-pos blocks-between-src-trgt))
      true
      false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
; path : Position Position ListOfPosition -> MaybePlan
; GIVEN:
; 1. the starting position of the robot,
; 2. the target position that robot is supposed to reach
; 3. A list of the blocks on the board
; RETURNS: a plan that, when executed, will take the robot from
; the starting position to the target position without passing over any
; of the blocks, or false if no such sequence of moves exists.
; DETAILS: If position (x1,y1) is considered white then the next position
;          (x1,y1+1) will be black. Thus the chessboard is formed of
;          squares alternating over the colors white and black.
;          If robot is on a white square it can never move to a
;          position that is a black square.
; STRATEGY: Call a more general function

(define (path start end blocks)
  (cond
    [(not (equal? (get-color start) (get-color end))) false]
    [else (complete-plan start end blocks (list empty empty))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
; get-color : Position -> String
; GIVEN: A position representing a square on the chessboard
; RETURNS: The String "White" if the square is white
;          The string "Black" if the square is black

;STRATEGY: Cases on Position

(define (get-color start)
  (if (or(and (even? (first start)) (even? (second start)))
         (and (odd? (first start)) (odd? (second start))))
      "white"
      "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; complete-plan : Position Position ListOfPosition Plan -> MaybePlan
; GIVEN: A source and target position
;        A list of blocks on the chessboard
;        A list of possible plans robot can execute to reach the target
; WHERE: The ListOfPlan is the list of all possible plans a robot can
;        execute so as to reach the target
;        First element of the Plan is is ListOfMove and second element
;        is the ListOfPosition list of squares a robot has already visited
; RETURNS: A Plan such that
;          the plan takes us to the target position if there are no blocks
;          on the way, otherwise false
; DETAILS: Given the list of blocks, we generate a list of possible plans,
;          that  take the robot to the target and recur on each of them
;          until a given plan works.
;          A a robot at position (x1,y1) can execute moves that take it the
;          position (x1+n,y1+m), (x1-n,y1-m), (x1-n,y1+m), (x1+n,y1-m) such that
;          n and m lie in positive integers (0,1,2,...n)         
; STRATEGY: Recurn on the ListOfPlan (list of all possible plans)
; HALTING MEASURE: Target position reached
; EXAMPLES: Refer Test Cases

(define (complete-plan src-pos trgt-pos blocks plan-visited)
  (cond
    [(equal? (eval-plan src-pos blocks (first plan-visited)) trgt-pos )
     (first plan-visited)]
    [else
     (first-success
      (lambda (pv) (complete-plan src-pos trgt-pos blocks pv))
      (legal-plans src-pos trgt-pos plan-visited blocks BASIC-MOVES))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; first-success : (A -> MaybePlan) ListOfPlan -> MaybePlan
; GIVEN: A function f that and a ListOfPlan
; RETURNS: First element of lop such that (f elt) is not false; else false
; STRATEGY: Use template for lopl on lop
; EXAMPLES: Refer Test Cases
(define (first-success f lop)
  (cond
    [(empty? lop) false]
    [else
     (local
       ((define output (f (first lop))))
       (if (not (false? output))
           output
           (first-success f (rest lop))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                TEST FO ROBOT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ROBOT SURROUNDED BY WALLS IN ALL FOUR DIRECTIONS
(define wall1
  '((0 3)(2 3)(4 3)
    (0 5)     (4 5)
    (0 7)(2 7)(4 7)))

; TWO WALLS BLOCKING THE ROBOT
(define two-walls
  '((0 3)(4 3)
    (0 5)(4 5)
    (0 7)(4 7)
    (0 9)(4 9)
    (0 11)(4 11)))

(define path1
  (list
 (list "se" 1)
 (list "nw" 1)
 (list "nw" 1)
 (list "nw" 1)
 (list "sw" 1)
 (list "nw" 1)
 (list "ne" 1)
 (list "ne" 1)
 (list "se" 1)
 (list "ne" 1)
 (list "se" 1)
 (list "ne" 1)
 (list "se" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "sw" 1)))

(define path2
  (list
 (list "ne" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "sw" 1)
 (list "se" 1)
 (list "se" 1)
 (list "ne" 1)
 (list "ne" 1)
 (list "se" 1)
 (list "se" 1)
 (list "ne" 1)
 (list "ne" 1)
 (list "ne" 1)
 (list "ne" 1)
 (list "nw" 1)
 (list "ne" 1)
 (list "nw" 1)))

(begin-for-test
  (check-equal?
   (path (list 2 5) (list 2 6) empty) false
   "Returns false when the robot is blocked")
  (check-equal?
   (path (list 2 5) (list 4 9) wall1) false
   "Returns false when the robot is blocked")
  (check-equal?
  (block-between-src-trgt? (list 1 1) empty (list "ne" 1)) false
  "Should return false when the block is not present at all")
  (check-equal?
   (robot-pos-after-next-move (list 1 1) empty) (list 1 1)
   "Should not move the robot when no move present")
  (check-equal?
   (path (list 2 5) (list 4 9) (rest wall1)) path1)
  (check-equal?
   (path (list -3 6) (list 7 6) two-walls) path2)
  (check-equal?
   (get-final-best-moves (list 1 1) (list 2 2) empty empty)
'())

)

