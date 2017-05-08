;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require "SWidget.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")

;; for test:
(require "ParticleWorld.rkt")
(require "Model.rkt")

(provide ControllerFactory%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;; CONTROLLER FACTORY                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ControllerFactory% is a
;; (new ControllerFactory% [w World<%>][m Model<%>])

(define ControllerFactory%
  (class* SWidget% ()

    (super-new)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    
    ; KeyEvent -> Void
    ; EFFECT: Hitting one of the following keys
    ;         causes a new viewer-controller to appear
    ;         in the center of the canvas:
    ;         "p" : Position controller
    ;         "v" : velocity controller
    ;         "x" : X controller
    ;         "y" : Y controller
    ;         "z" : XY controller
    (define/override (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        [(key=? kev "x") (add-viewer XController%)]
        [(key=? kev "y") (add-viewer YController%)]
        [(key=? kev "z") (add-viewer XYController%)]
        ))

    ; Controller<%> -> Void
    ; EFFECT: add a new Controller to the world
    (define (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

(begin-for-test
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT))
         (f (new ControllerFactory% [m m][w w])))
    
    (check-equal?
     (begin
       (send f after-key-event "v")
       (length (send w for-test:widgets)))
     1)
    (check-equal?
     (begin
       (send f after-key-event "p")
       (length (send w for-test:widgets)))
     2)
    (check-equal?
     (begin
       (send f after-key-event "x")
       (length (send w for-test:widgets)))
     3)
    (check-equal?
     (begin
       (send f after-key-event "y")
       (length (send w for-test:widgets)))
     4)
    (check-equal?
     (begin
       (send f after-key-event "z")
       (length (send w for-test:widgets)))
     5)
    ))


