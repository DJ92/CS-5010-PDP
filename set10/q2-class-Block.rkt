#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "q2-interfaces.rkt")
(require "sets.rkt")

(provide
 Block%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  CLASSES                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Block% class

;; A Block is a (new Block% [x Int][y Int]
;;               [selected? Boolean][saved-mx Integer][saved-my Integer]
;;               [blocks ListOfBlock<%>])

;; the Ball is a stateful widget and can be selected and dragged

(define Block%
  (class* object% (Block<%>)

    ; the position of the block   
    (init-field x y)

    ; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; blocks's center.  Else any value.
    (init-field [saved-mx 0])
    (init-field [saved-my 0])

    ; the list of blocks that registed to send info
    (init-field [blocks empty])
    ; the block's teammates
    (field [teammates empty])

    ; the block's image
    ; orginally, it is green. And turns red when selected
    (field [l 20]) 
    (field [LEADER-IMG (rectangle l l "outline" "red")])
    (field [BLOCK-IMG (rectangle l l "outline" "green")])
    
    (super-new)

    ; -> ListOfBlock<%>
    ; RETURNS: the teammates of this block
    (define/public (get-team) teammates)
    
    ; Block<%> ListOfBlock<%> -> Void
    ; GIVEN: the leader and the leader's teammates
    ; EFFECT: updates the block's teammates
    ;         after the leader has collected some new teammates
    (define/public (update-teammates leader lst)
      (set! teammates (cons leader (remove this lst))))

    ; Block<%> ListOfBlock<%> -> Void
    ; GIVEN: the leader's list of blocks which are not teammates
    ; EFFECT: updates the block's list of blocks which are not teammates
    ;         after the leader has collected some new teammates
    (define/public (update-blocks lst)
      (set! blocks lst))

    ; Block<%> -> Void
    ; GIVEN: An Block<%>
    ; EFFECT: registers the block to other blocks
    (define/public (register b)
      (set! blocks (cons b blocks)))

    ; Int Int -> Void
    ; EFFECT: updates the blocks's position since the leader moved
    (define/public (update-pos mx my)
      (set! x (- mx saved-mx))
      (set! y (- my saved-my)))

    ; NonNegInt NonNegInt -> Void
    ; GIVEN: the new mouse position
    ; EFFECT: updates the saved-mx and saved-my
    (define/public (reset-mouse mx my)
      (set! saved-mx (- mx x))
      (set! saved-my (- my y)))

    ; NonNegInt NonNegInt -> Void
    ; GIVEN: the mouse position
    ; EFFECTS: if the mouse is in the block
    ;          1. updates mx and my and set selected? true
    ;          2. let all the blocks in the block list update the mouse position
    ;          else return this
    (define/public (after-button-down mx my)
      (if (in-block? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))
            (for-each
             (lambda (b) (send b reset-mouse mx my))
             teammates))
          this))  

    ; NonNegInt NonNegInt -> Void
    ; GIVEN: the mouse position
    ; EFFECTS: if the mouse is in the block
    ;          then set selected? false
    ;          else return this
    (define/public (after-button-up mx my)
      (if (in-block? mx my)
          (set! selected? false)
          this))
    
    ; NonNegInt NonNegInt -> Void
    ; GIVEN: the mouse position
    ; EFFECTS: if the blocks is selected
    ;          1. updates mx and my 
    ;          2. let all teammates update themselves position
    ;          3. call add-newmates to dealt with whether there is a new mate.
    ;          4. let all teammates update mouse position
    ;          else return this
    (define/public (after-drag mx my)
        (if selected?
            (begin
              (set! x (- mx saved-mx))
              (set! y (- my saved-my))
              (for-each
               (lambda (b) (send b update-pos mx my))
               teammates)
              (add-newmates)
              (for-each
               (lambda (b) (send b reset-mouse mx my))
               teammates))
            this))

    ; -> Void
    ; EFFECT: if this block has collected some new blocks, call add-teammates
    ;         else return this
    (define (add-newmates)
      (let ((newmates
             (filter
              (lambda (b) (send b intersect? x y))
              blocks)))
        (if (empty? newmates)
            this
            (add-teammates newmates))))
    
    ; ListOfBlock<%> -> Void
    ; GIVEN: a list of new teammates being collected by the leader after drag
    ; EFFECT: 1. add the new teammates and every new teammate's teammates
    ;            to leader's teammates list
    ;         2. let the leader's teammates update their teammates list
    ;         3. remove the new teammates and every new teammate's teammates
    ;            from the leader's list of blocks
    ;         4. let the leader's teammates get the same block list
    (define (add-teammates newmates)
      (begin
        (new-team newmates)
        (for-each
         (lambda (b) (send b update-teammates this teammates))
         teammates)
        (for-each
         (lambda (b) (set! blocks (remove b blocks)))
         teammates)
        (for-each
         (lambda (b) (send b update-blocks blocks))
         teammates)))

    ; new-team: ListOfBlock<%> -> Void
    ; GIVEN: the list of blocks which the leader directly get into touch with
    ; EFFECT: add all the new teammates and their teammates to the leader's team
    (define (new-team newmates)
      (for-each
         (lambda (b) (add-teammate b))
         (foldr
          (lambda (b lst) (cons b (append (send b get-team) lst)))
            empty
            newmates)))
    
    ; add-teammate: Block<%> -> Void
    ; EFFECT: adds the given block to this block's team
    (define/public (add-teammate b)
      (set! teammates
            (if (my-member? b teammates)
                teammates
                (cons b teammates))))

    ; Scene -> Scene
    ; RETURNS: an image of the block painted on the previous scene
    (define/public (add-to-scene s)
      (if selected?
          (place-image LEADER-IMG x y s)
          (place-image BLOCK-IMG x y s)))

    ; Int Int -> Boolean
    ; GIEVEN: the x y position of the leader
    ; RETURNS: true iff the block intersects with the leader
    (define/public (intersect? leader-x leader-y)
      (and
       (<= (abs (- leader-x x)) l)
       (<= (abs (- leader-y y)) l)))

    ; -> Int
    ; RETURNS: the x y position of the block
    (define/public (block-x) x)
    (define/public (block-y) y)

    ; -> Block<%>
    ; RETURNS: the block unchanged
    (define/public (after-tick) this)
    
    ; KeyEvent -> Block<%>
    ; GIVEN: a key event
    ; RETURNS: the block unchanged
    (define/public (after-key-event kev) this)

    ; NonNegInt NonNegInt -> Boolean
    ; GIVEN: the mouse position
    ; RETURNS: true iff the mouse is in the block
    (define (in-block? mx my)
      (and
       (<= (abs (- mx x)) (/ l 2))
       (<= (abs (- my y)) (/ l 2))))

    ; -> ListOfBlock<%>
    (define/public (for-test:blocks) blocks)
    ; -> Boolean
    (define/public (for-test:selected?) selected?)
    ; -> Int
    (define/public (for-test:saved-mx) saved-mx)
    (define/public (for-test:saved-my) saved-my)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define unselected-b1-scene
  (place-image (rectangle 20 20 "outline" "green")
               80 40
               EMPTY-CANVAS))

(define selected-b0-scene
  (place-image (rectangle 20 20 "outline" "red")
               40 40
               EMPTY-CANVAS))

(begin-for-test
  (local
    ((define b0 (new Block% [x 40][y 40]))
     (define b1 (new Block% [x 80][y 40][blocks (list b0)]))
     (define b2 (new Block% [x 120][y 40][blocks (list b0 b1)]))
     (define b3 (new Block% [x 160][y 40][blocks (list b0 b1 b2)]))
     (define b4 (new Block% [x 200][y 40][blocks (list b0 b1 b2 b3)])))
    
    ;;tests for register:
    (check-equal?
     (begin (send b0 register b1)
            (send b0 register b2)
            (send b0 register b3)
            (send b1 register b2)
            (send b1 register b3)
            (send b2 register b3)
            (send b0 for-test:blocks))
     (list b3 b2 b1))
    (check-equal?
     (begin (send b1 for-test:blocks))
     (list b3 b2 b0))
    (check-equal?
     (begin (send b2 for-test:blocks))
     (list b3 b0 b1))
    (check-equal?
     (begin (send b3 for-test:blocks))
     (list b0 b1 b2))
    
    ;;tests for after-button-down:
    ;; the mouse not in the b0
    (check-equal?
     (begin (send b0 after-button-down 20 40)
            (send b1 for-test:saved-mx))
     0)
    ;; the mouse is in the b0
    (set-equal?
     (begin (send b0 after-button-down 30 40)
            (send b1 for-test:blocks))
     (list b0 b2 b3 b4))

    ;;tests for add-to-scene :
    ;; b0 is selected
    (check-equal?
     (send b0 add-to-scene EMPTY-CANVAS)
     selected-b0-scene)
    ;; b1 is unselected
    (check-equal?
     (send b1 add-to-scene EMPTY-CANVAS)
     unselected-b1-scene)
    
    ;; tests for after-button-up:
    ;; the mouse not in the b0
    (check-equal?
     (begin (send b0 after-button-up 20 40)
            (send b0 for-test:selected?))
     true)
    ;; the mouse is in the b0
    (check-equal?
     (begin (send b0 after-button-up 30 40)
            (send b0 for-test:selected?))
     false)

    ;; tests for after-drag:
    ;; b0 is unselected now
    (check-equal?
     (begin (send b0 after-drag 20 40)
            (send b0 block-x))
     40)
    ;; b0 is selected but not contract with other blocks after drag
    (check-equal?
     (begin (send b0 after-button-down 30 40)
            (send b0 after-drag 30 60)
            (send b0 block-y))
     60)
    ;; b0 is selected and contract with b1 after drag
    (check-equal?
     (begin (send b0 after-drag 50 40)
            (send b0 block-x))
     60)
    (check-equal?
     (send b1 for-test:saved-my)
     0)
    (check-equal?
     (begin (send b0 get-team))
     (list b1))
    (check-equal?
     (begin (send b0 for-test:blocks))
     (list b3 b2))
    (check-equal?
     (begin (send b1 get-team))
     (list b0))
    (check-equal?
     (begin (send b1 for-test:blocks))
     (list b3 b2))
    
    ;; b0 now is with one teammate b1, after drag they should move together 
    (check-equal?
     (begin (send b0 after-drag 60 50)
            (send b0 after-button-up 70 40)
            (send b0 block-x))
     70)
    (check-equal?
     (begin (send b0 block-y))
     50)
    (check-equal?
     (begin (send b1 block-x))
     90)
    (check-equal?
     (begin (send b1 block-y))
     50)

    ;; b1 now is with one teammate b0, after drag they should move together
    (check-equal?
     (begin (send b1 after-button-down 90 50)
            (send b1 after-drag 90 90)
            (send b1 after-button-up 80 100)
            (send b0 block-x))
     70)
    (check-equal?
     (begin (send b0 block-y))
     90)
    (check-equal?
     (begin (send b1 block-x))
     90)
    (check-equal?
     (begin (send b1 block-y))
     90)
    
    ;; select and drag b2 to get into touch with b0
    (check-equal?
     (begin (send b2 after-button-down 110 30)
            (send b2 after-drag 100 60)
            (send b2 after-button-up 110 60)
            (send b2 block-x))
     110)
    (check-equal?
     (begin (send b2 block-y))
     70)
    (check-equal?
     (begin (send b0 get-team))
     (list b2 b1))
    (check-equal?
     (begin (send b0 for-test:blocks))
     (list b3))
    (check-equal?
     (begin (send b1 get-team))
     (list b2 b0))
    (check-equal?
     (begin (send b1 for-test:blocks))
     (list b3))
    (check-equal?
     (begin (send b2 get-team))
     (list b0 b1))
    (check-equal?
     (begin (send b2 for-test:blocks))
     (list b3))
    (check-equal?
     (begin (send b3 get-team))
     empty)
    (check-equal?
     (begin (send b3 for-test:blocks))
     (list b0 b1 b2))

    ;; select and drag b1 to get into touch with b3
    (check-equal?
     (begin (send b1 after-button-down 90 90)
            (send b1 after-drag 140 20)
            (send b1 after-button-up 140 20)
            (send b1 block-x))
     140)
    (check-equal?
     (begin (send b1 block-y))
     20)
    (check-equal?
     (begin (send b0 get-team))
     (list b1 b3 b2))
    (check-equal?
     (begin (send b0 for-test:blocks))
     empty)
    (check-equal?
     (begin (send b1 get-team))
     (list b3 b2 b0))
    (check-equal?
     (begin (send b1 for-test:blocks))
     empty)
    (check-equal?
     (begin (send b2 get-team))
     (list b1 b3 b0))
    (check-equal?
     (begin (send b2 for-test:blocks))
     empty)
    (check-equal?
     (begin (send b3 get-team))
     (list b1 b2 b0))
    (check-equal?
     (begin (send b3 for-test:blocks))
     empty)

    ;; select and drag b4 to get into touch with b2 and b3
    (check-equal?
     (begin (send b4 after-button-down 200 40)
            (send b4 after-drag 180 20)
            (send b4 after-button-up 180 20)
            (send b4 block-x))
     180)
    (check-equal?
     (begin (send b4 block-y))
     20)
    (set-equal?
     (begin (send b0 get-team))
     (list b4 b1 b3 b2))
    (check-equal?
     (begin (send b0 for-test:blocks))
     empty)
    (set-equal?
     (begin (send b1 get-team))
     (list b4 b3 b2 b0))
    (check-equal?
     (begin (send b1 for-test:blocks))
     empty)
    (set-equal?
     (begin (send b2 get-team))
     (list b4 b1 b3 b0))
    (check-equal?
     (begin (send b2 for-test:blocks))
     empty)
    (set-equal?
     (begin (send b3 get-team))
     (list b4 b1 b2 b0))
    (check-equal?
     (begin (send b3 for-test:blocks))
     empty)
    
    ;; tests for after-key-event:
    (check-equal?
     (begin (send b0 after-key-event "b"))
     b0)

    ;; tests for after-tick :
    (check-equal?
     (send b0 after-tick)
     b0)
  ))