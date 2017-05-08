;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 1: Exercise 13
;;
;; Define a function that consumes two numbers, x and y, and
;; that computes the distance of point (x,y) to the origin.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")
(provide distance-to-origin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; POINT
(define-struct point(xpoint ypoint))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Point is (make-point Integer Integer)

;; INTERPRETATIONS :
;;  - xpoint is the x co-ordinate
;;  - ypoint is the y co-ordinate

;; DESTRUCTOR TEMPLATE :
;;
;; point-fn : Point -> ??

(define (point-fn po)
  (...(point-x-point point)
      (point-y-point point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; distance-to-origin : Integer Integer -> PosInt
;; GIVEN: a Point with x and y co-ordinates
;; RETURNS: distance from the specified point to the origin (0,0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (distance-to-origin (make-point 6 -8))
;  10

;; (distance-to-origin (make-point -3 4))
;  5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test
  (check-equal? (distance-to-origin (make-point 6 -8)) 10 "The distance from origin (0,0) to (6, -8) is 10."))

(begin-for-test
  (check-equal? (distance-to-origin (make-point -3 -4)) 5 "The distance from origin (0,0) to (-3, -4) is 5."))

(begin-for-test
  (check-equal? (distance-to-origin (make-point -3 4)) 5 "The distance from origin (0,0) to (-3, 4) is 5."))

(begin-for-test
  (check-equal? (distance-to-origin (make-point 6 8)) 10 "The distance from origin (0,0) to (6, 8) is 10."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :
;; Function Composition

(define (distance-to-origin po)
  (sqrt
   (+ (sqr (point-xpoint po))
      (sqr (point-ypoint po)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;