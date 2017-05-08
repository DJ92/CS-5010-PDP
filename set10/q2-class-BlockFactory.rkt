#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "q2-class-Block.rkt")
(require "WidgetWorks.rkt")

(provide
 BlockFactory%)

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define INIT-BLOCK-X (/ CANVAS-WIDTH 2))
(define INIT-BLOCK-Y (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  CLASSES                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The BlockFactory% class

;; A BlockFactory is a
;; (new BlockFactory% [blocks ListOfBlock<%>][x Int][y Int])

;; the BlockFactory is a stateful widget

;; accepts "b" key events and adds a block to the world.
;; gets the world as an init-field

(define BlockFactory%
  (class* object% (SWidget<%>)

    (init-field world)           ; the world to which the factory adds blocks
    (init-field [blocks empty])  ; the list of blocks that live in the world
    
    ; the position that the block initially pops up
    ; which is the center of the canvas
    ; if there's no button-down or button-up yet
    (init-field [x INIT-BLOCK-X])
    (init-field [y INIT-BLOCK-Y]) 

    (super-new)

    ; KeyEvent -> Void
    ; EFFECTS:1. add a new block to the world as a stateful widget
    ;         2. let the existed blocks register the new block
    ;         3. updates the blockfactory's block list
    ;            which records all blocks live in the world
    (define/public (after-key-event kev)
      (local
        ((define new-block (new Block% [x x][y y][blocks blocks])))
        (cond
          [(key=? kev "b")
           (begin
             (send world add-stateful-widget new-block)
             (for-each
              (lambda (b) (send b register new-block))
              blocks)
             (set! blocks (cons new-block blocks)))]
          [else this])))

    ; NonNegInt NonNegInt -> BlockFactory%
    ; GIVEN: the mouse position
    ; EFFECT: updates the x y position where a new block would pops up
    (define/public (after-button-down mx my)
      (begin
        (set! x mx)
        (set! y my)
        this))

    ; NonNegInt NonNegInt -> BlockFactory%
    ; GIVEN: the mouse position
    ; EFFECT: updates the x y position where a new block would pops up
    (define/public (after-button-up mx my)
      (begin
        (set! x mx)
        (set! y my)
        this))
    
    ;; the Ball Factory has no other behavior
    (define/public (after-drag mx my) this)
    (define/public (add-to-scene s) s)
    (define/public (after-tick) this)

    ; -> NonNegInt
    (define/public (for-tests:x) x)
    (define/public (for-tests:y) y)
    ; -> ListOfBlock<%>
    (define/public (for-tests:blocks) blocks)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define the-world
       (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define the-factory
       (new BlockFactory% [world the-world])))
    
    ;; tests for after-key-event:
    ;; other key event "s"
    (check-equal?
     (begin (send the-factory after-key-event "s"))
     the-factory)
    ;; key event "b"
    (check-equal?
     (begin (send the-factory after-key-event "b")
            (send (first (send the-factory for-tests:blocks))
                  block-x))
     INIT-BLOCK-X)
    ;; another key event "b"
    (check-equal?
     (begin (send the-factory after-key-event "b")
            (length (send the-factory for-tests:blocks)))
     2)
    
    ;;tests for after-button-down:
    (check-equal?
     (begin (send the-factory after-button-down 20 40)
            (send the-factory for-tests:x))
     20)
    (check-equal?
     (send the-factory for-tests:y)
     40)
    
    ;; tests for after-button-up:
    (check-equal?
     (begin (send the-factory after-button-up 60 80)
            (send the-factory for-tests:x))
     60)
    (check-equal?
     (send the-factory for-tests:y)
     80)
    
    ;; tests for after-drag:
    (check-equal?
     (send the-factory after-drag 20 40)
     the-factory)

    ;;tests for add-to-scene:
    (check-equal?
     (send the-factory add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS)

    ;; tests for after-tick :
    (check-equal?
     (send the-factory after-tick)
     the-factory)
  ))