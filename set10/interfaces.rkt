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

;; Following are the interfaces for the toys.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(provide PlaygroundState<%>
         Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 INTERFACES                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The PlayGround% implements the PlaygroundState<%> interface

(define PlaygroundState<%>
  (interface (SWidget<%>) ;; include all the methods in SWidget<%>. 
    
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

;; Every object that lives in the playground must implement the Toy<%>
;; interface.

;; The Toy% implements the Toy<%> interface.

(define Toy<%> 
  (interface (SWidget<%>)  ;; include all the methods in SWidget<%>. 
 
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
    ;; for a football, it is the scaling factor of the football (in
    ;; arbitrary units; bigger is more)
    toy-data
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;