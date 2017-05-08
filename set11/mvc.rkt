;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;; PROBLEM SET 11                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (mvc.rkt).
;; GOALS:
;; The goal of this problem set is to give you experience with inheritance
;; and the model-viewer-controller architecture.

;; We will produce 5 viewer-controllers:
;; 1. position controller:
;;   using the arrow keys to move the particle in the x or y direction.
;; 2. velocity controller:
;;   using the arrow keys to alter the velocity of the particle.
;; 3. XY controller:
;;   shows a representation of the particle bouncing in the rectangle.
;; 4. X controller:
;;   displays only the x coordinate of the particle's motion.
;; 5. Y controller: works in the y direction.

;; Hitting one of the following keys causes a new controller to appear
;; in the center of the canvas:
;; "p" : Position controller
;; "v" : velocity controller
;; "x" : X controller
;; "y" : Y controller
;; "z" : XY controller

;; HOW TO USE: (run rate). Typically, (run 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "ParticleWorld.rkt")
(require "Model.rkt")
(require "ControllerFactory.rkt")

(provide
 run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CONSTANTS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                FUNCTIONS                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run : PosReal -> Void
;GIVEN: a frame rate, in sec/tick
;EFFECT: Creates and runs the MVC simulation with the given frame rate.
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT))
         (f (new ControllerFactory% [m m][w w])))
      (send w add-widget f)
      (send w run rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(run 0.5)
