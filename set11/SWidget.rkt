;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")

(provide SWidget%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;; CONSTANTS                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                ;; SWIDGET                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SWidget% is a (new SWidget%)

;; A SWidget is a stateful widget which is draggable

(define SWidget%
  (class* object% (SWidget<%>)
    (super-new)

    ; -> Int
    ; the x y coordinates of the center of the SWidget
    (init-field [x (/ CANVAS-WIDTH 2)])
    (init-field [y (/ CANVAS-HEIGHT 2)])

    ; -> Bool
    ; is this selected? Default is false.
    (init-field [selected? false])

    ; -> PosInt
    ;; if this is selected, the position of
    ;; the last button-down event inside this, relative to the
    ;; blocks's center.  Else any value.
    (init-field [saved-mx 0])
    (init-field [saved-my 0])

    ; -> Nat
    ;; the boundrary of the field
    (init-field [width 200])
    (init-field [height 150])

    ; -> Void
    ; EFFECT: no change after tick
    (define/public (after-tick) empty)
    
    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    (define/public (after-key-event kev)
      empty)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    (define/public (after-button-down mx my) empty)
    (define/public (after-drag mx my) empty)
    (define/public (after-button-up mx my) empty)

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    (define/public (add-to-scene scene)
      (place-image (to-image) x y scene))

    ; -> Void
    ; RETURNS: the image which should appear inside the Widget
    (define/public (to-image) empty-image)

    ; Nat Nat -> Boolean
    ; GIVEN: a mouse location
    ; RETRUNS: true iff the given location is inside the Widget
    (define/public (in-object? other-x other-y)
      (and
        (<= (- x (/ width 2))  other-x (+ x (/ width 2)))
        (<= (- y (/ height 2)) other-y (+ y (/ height 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define EMPTY-SCENE (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define test-img
  (overlay (rectangle 150 50 "outline" "blue")
           (place-image
            (overlay (circle 2 "solid" "black")
                     (circle 10 "solid" "red"))
            80 25
            (rectangle 150 50 "outline" "blue"))))

(begin-for-test
  (let*
      ((s (new SWidget%)))
    
    (check-equal?
     (send s after-tick)
     empty)
    
    (check-equal?
     (send s after-key-event "v")
     empty)

    (check-equal?
     (begin
       (send s after-button-down 300 250)
       (send s after-drag 300 250)
       (send s after-button-up 300 250))
     empty)

    (check-equal?
     (send s add-to-scene EMPTY-SCENE)
     EMPTY-SCENE)

    (check-equal?
     (send s in-object? 300 250)
     true)

    ))