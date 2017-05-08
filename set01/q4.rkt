;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 4: Exercise 21
;;
;; Define the function string-insert, which consumes a string and a number i and which
;; inserts "_" at the ith position of the string. Assume i is a number between 0 and
;; the length of the given string (inclusive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")
(provide string-insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :
;;
;; INPUT
(define-struct input(sample-string index))

;; CONSTRUCTOR TEMPLATE :
;;
;; (make-input String PosInt)

;; INTERPRETATIONS :
;; - string is the input string
;; - index is the specified position to insert "_" at.

;; DESTRUCTOR TEMPLATE :
;;
;; input-fn; Input -> ??

(define (input-fn in)
  (...(input-sample string input)
      (input-index input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; string-insert : PosInt String -> String
;; GIVEN : A string and an index
;; RETURNS: string with an inserted "_" at the specified index.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :
;;
;; (string-insert (make-input "Dheeraj is awesome." 4))
; "Dhee_raj is awesome."

;; string-insert (make-input "" 4))
; "_"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test (check-equal? (string-insert (make-input "Dheeraj is awesome." 4)) "Dhee_raj is awesome.")
                (check-equal? (string-insert (make-input "Dheeraj is awesome." 44)) "Index out of range.")
                (check-equal? (string-insert (make-input "" 4)) "_"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY :
;; Function Composition

(define (string-insert in)
  (cond[(<= (string-length (input-sample-string in)) 0) (string-append "_")]
       [(>= (input-index in) (+ (string-length (input-sample-string in)) 1)) (string-append "Index out of range.")]
       [(and (<= (input-index in) (string-length (input-sample-string in))) (> (input-index in) 0)) (string-append
   (substring (input-sample-string in) 0 (input-index in))
   "_"
   (substring (input-sample-string in) (input-index in) (string-length (input-sample-string in))))]))
