;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "interfaces.rkt")
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "PlayGround.rkt")
(require "Square.rkt")
(require "Throbber.rkt")
(require "Clock.rkt")
(require "Football.rkt")

(provide
 make-square-toy
 make-throbber
 make-clock
 make-football
 make-playground-state
 run
 PlaygroundState<%>
 Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTANTS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIMENSIONS OF THE CANVAS
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                FUNCTIONS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum PosInt -> ?? 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;; creates and runs a world in which square toys travel at the given
;; speed. 
;; EFFECT: run an initial world at the given frame rate
;; STRATEGY: combine simpler functions

(define (run rate square-speed)
  (local
    [(define world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define target (make-playground-state square-speed))]
    (send world add-stateful-widget target)
    (send world run rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;