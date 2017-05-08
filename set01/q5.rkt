;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 5: Exercise 22
;;
;; Define the function string-delete, which consumes a string and a number i and which
;; deletes the ith position from str. Assume i is a number between 0 (inclusive) and
;; the length of the given string (exclusive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")
(provide string-delete)

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
;; - index is the specified position to delete character from.

;; DESTRUCTOR TEMPLATE :
;;
;; input-fn; Input -> ??

(define (input-fn in)
  (...(input-sample string input)
      (input-index input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; string-delete : PosInt String -> String
;; GIVEN : A string and an index
;; RETURNS: string with a deleted character at the specified index.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (make-input "Dheeraj is awesome." 4))
; "Dheraj is awesome."

;; (make-input "" 22))
; "Empty String."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :
  
(begin-for-test (check-equal? (string-delete (make-input "Dheeraj is awesome." 4)) "Dheraj is awesome.")
                (check-equal? (string-delete (make-input "Dheeraj is awesome." 22)) "Index out of range.")
                (check-equal? (string-delete (make-input "" 22)) "Empty String."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY :
;; Function Composition

(define (string-delete in)
  (cond[(<= (string-length (input-sample-string in)) 0) (string-append "Empty String.")]
       [(>= (input-index in) (string-length (input-sample-string in))) (string-append "Index out of range.")]
       [(and (<= (input-index in) (string-length (input-sample-string in))) (> (input-index in) 0)) (string-append
   (substring (input-sample-string in) 0 (- (input-index in) 1))
   (substring (input-sample-string in) (input-index in) (string-length (input-sample-string in))))]))
