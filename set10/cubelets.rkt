;; (cubelets.rkt).
;; GOALS:

;; Produce a new toy inspired by Cubelets,
;; which are square blocks that stick together.

;; When types "b", a new block pops up on the screen
;; at the location of the last button-down or button-up.
;; the child can move it around using Smooth Drag.

;; If a block is dragged so that it contacts or overlaps another block,
;; the two blocks become connected. We say that the blocks are teammates.

;; The property of being a teammate is symmetric and transitive.
;; So if block A is moved to touch block B,
;; then a new team is formed consisting of A and all its teammates,
;; and B and all its teammates.

;; When a block is moved, all its teammates move along with it.

;; Only the selected block accumulates teammates.
;; we call the selected block the "leader."

;; HOW TO USE:
;; 1. start with (run framerate). Typically (run 0.2)
;; 2. press "b" key to add cubes
;; 3. drag cubes to make teams

#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "q2-class-BlockFactory.rkt")
(require "q2-class-Block.rkt")

(provide
 make-block)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTANTS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                FUNCTIONS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosReal -> StatefulWorld<%>
;; GIVEN: Any Value 
;; Returns: the final state of the World.
(define (run rate)
  (send (initial-world) run rate))

;; initial-world : -> StatefulWorld<%>
;; GIVEN: no arguments
;; RETURNS: a StatefulWorld<%>
(define (initial-world)
  (local
    ((define the-world
       (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define the-factory
       (new BlockFactory% [world the-world])))
    (begin
      (send the-world add-stateful-widget the-factory)
      the-world)))

;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is
;;        the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
    (define (make-block x y blocks)
      (new Block% [x x][y y][blocks blocks]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  TESTS                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (local
    ((define test-world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define test-factory (new BlockFactory% [world test-world]))
     (define b0 (make-block 40 40 empty))
     (define b1 (make-block 80 80 (list b0))))
    
    ;;tests for initial-world :
    (check-equal?
     (begin
       (initial-world)
       (send test-factory for-tests:blocks))
     empty)
    
    ;;tests for make-block :
    (check-equal?
     (send b0 for-test:blocks)
     empty)
    (check-equal?
     (send b0 block-x)
     40)
    (check-equal?
     (send b0 block-y)
     40)
    (check-equal? 
     (send b1 for-test:blocks)
     (list b0))
    (check-equal?
     (send b1 block-x)
     80)
    (check-equal?
     (send b1 block-y)
     80)
    ))


