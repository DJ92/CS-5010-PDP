;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;; REQUIRED FOR HOMEWORK SUBMISSION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require "SWidget.rkt")

(provide Handle%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;; HANDLE                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Handle% is a (new Handle% [controller Controller<%>])

(define Handle%
  (class* SWidget% ()
    (super-new)

    ; -> Controller%
    ; the controller to which the handle is connected
    (init-field controller)

    ; -> Int
    ; the x y coordinates of the center of the handle
    (inherit-field x)
    (inherit-field y)
    
    ; -> Bool
    ; is this selected? Default is false.
    (inherit-field selected?)

    ; -> Nat
    ;; the boundrary of the field
    (inherit-field width)
    (inherit-field height)

    (set! width 10)       ; width of the handle
    (set! height 10)      ; height of the handle
    (set! x (/ width 2))  ; x co-ord of the center of the handle
    (set! y (/ height 2)) ; y co-ord of the center of the handle

    ; -> Image
    ; RETURNS: the handle's image
    (define/override (to-image)
      (rectangle width height "outline" (current-color)))

    ; -> String
    ; RETURNS: if the handle is selected, return red, else black
    (define (current-color)
      (if selected? "red" "black"))

    ; For Test:
    ; -> Void
    (define/public (for-test:set-selected?)
      (set! selected? true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   TESTS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-img1 (rectangle 10 10 "outline" "black"))
(define test-img2 (rectangle 10 10 "outline" "red"))

(begin-for-test
  (let*
      ((h (new Handle% [controller empty])))
    
    (check-equal?
     (send h to-image)
     test-img1)
    (check-equal?
     (begin
       (send h for-test:set-selected?)
       (send h to-image))
     test-img2)
    ))