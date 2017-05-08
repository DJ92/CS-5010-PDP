#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(provide
 Block<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 INTERFACES                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Block<%>
  (interface (SWidget<%>)

    ; -> ListOfBlock<%>
    ; RETURNS: the teammates of this block
    get-team

    ; Block<%> ListOfBlock<%> -> Void
    ; GIVEN: the leader and the leader's teammates
    ; EFFECT: updates the block's teammates
    ;         after the leader has collected some new teammates
    update-teammates

    ; Block<%> ListOfBlock<%> -> Void
    ; GIVEN: the leader's list of blocks which are not teammates
    ; EFFECT: updates the block's list of blocks which are not teammates
    ;         after the leader has collected some new teammates
    update-blocks

    ; Block<%> -> Void
    ; GIVEN: An Block<%>
    ; EFFECT: registers the block to other blocks
    register
    
    ; Int Int -> Void
    ; EFFECT: updates the blocks's position since the leader moved
    update-pos

    ; NonNegInt NonNegInt -> Void
    ; GIVEN: the new mouse position
    ; EFFECT: updates the saved-mx and saved-my
    reset-mouse

    ; Block<%> -> Void
    ; EFFECT: adds the given block to this block's team
    add-teammate
    
    ; Int Int -> Boolean
    ; GIEVEN: the x y position of the leader
    ; RETURNS: true iff the block intersects with the leader
    intersect?

    ; -> Int
    ; RETURNS: the x y position of the block
    block-x
    block-y
    
    ))
