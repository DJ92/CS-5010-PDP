;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;; Interfaces:

(provide
 World<%>
 SWidget<%>
 Controller<%>
 Model<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;; WORLD                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define World<%>
  (interface ()

    ; SWidget<%> -> Void
    add-widget                          ; all the widgets are stateful

    ; PosReal -> Void
    run
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;; SWIDGET                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SWidget<%>
  (interface ()
    after-tick             ; -> Void
    add-to-scene           ; Scene -> Scene
    after-button-up        ; Nat Nat -> Void
    after-button-down      ; Nat Nat -> Void
    after-drag             ; Nat Nat -> Void
    after-key-event        ; KeyEvent -> Void
    to-image               ; -> Void
    in-object?             ; Nat Nat -> Boolean
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;; CONTROLLER                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Controller<%>    
  (interface (SWidget<%>)

    ; -> Scene
    ; RETURNS: the view inside the controller
    data-image
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;; MODEL                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Model<%>
  (interface ()

    ;; -> Void
    after-tick
    
    ; Controller% -> Void
    ; Registers the given controller to receive signal
    register

    ; -> Int
    ; RETURNS: the particle's x y coordinates and velocity
    return-particle-x
    return-particle-y
    return-particle-vx
    return-particle-vy

    ; Int -> Void
    ; EFFECT: update the particle's x y coordinates and velocity
    update-particle-x
    update-particle-y
    update-particle-vx
    update-particle-vy

    ))