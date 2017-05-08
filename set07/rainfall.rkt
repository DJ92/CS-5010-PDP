;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set: 07

;; Question 3:
;;
;; Design a program that consumes a list of numbers representing daily rainfall
;; amounts . The list may contain the number -999 indicating the end of the data
;; of interest. Produce the average of the non-negative values in the list up
;; to the first -999 (if it shows up). There may be negative numbers other than
;; -999 in the list. Deliver your program by providing a function called
;; rainfall in a file named rainfall.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide
 rainfall)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LIST OF NUMBER :

;; A ListOfNumber is one of -

;; - empty
;; Interpretation : list of numbers is empty when a given list has no numbers. 

;; - (cons n lon)
;; Interpretation : (cons n lon) represents a sequence of Numbers representing
;;                  daily rainfall amounts, whose first element n is a Number
;;                  and other numbers are represented by lon.

;; lon-fn : ListOfNumber -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;             (first lon)
;;             (lon-fn (rest lon)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                   ;; CONSTANTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; LIST OF RAINFALL AMOUNTS
(define RAINFALL-AMT (list 15 -435 -67 65 7876 -999 43543 435 65 -7799 98 -999))

; AVERAGE OF ALL THE POSITIVE AMOUNTS OF RAINFALL TILL END OF DATA
(define AVG 2652)

; END OF DATA OF INTEREST VALUE
(define END-OF-DATA -999)

; FLAG REPRESENTING THE STATE WHERE END OF DATA IS ENCOUNTERED
(define TRUE-FLAG 1)

; FLAG REPRESENTING THE STATE WHERE END OF DATA IS NOT ENCOUNTERED
(define FALSE-FLAG 0)

; BASE VALUE
(define SUM 0)

; CONSTANT TO CHECK WHETHER A NUMBER IS POSITIVE OR NEGATIVE
(define ZERO 0)

; AVERAGE WHEN LIST IS EMPTY
(define DEFAULT-AVG 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ; FUNCTIONS CALCULATING AVERAGE RAINFALL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;rainfall: ListOfNumber -> NonNegReal
;GIVEN: A list of numbers that represents the daily rainfall amounts
;RETURNS: The average of all non-negative real numbers in the list till
;         end of data is encountered i.e on first occurence of -999
;         iff the list is not empty.
;STRATEGY: Cases on lon
;EXAMPLE : Refer test cases

(define (rainfall lon)
  (if (empty? lon)
      DEFAULT-AVG
      (get-average lon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-average: ListOfNumber -> NonNegReal
;GIVEN: A list of numbers lon that represents the daily rainfall amounts
;WHERE: the given list is not empty
;RETURNS: The average of all non-negative real numbers in the list till
;         end of data is encountered i.e on first occurence of -999
;STRATEGY: Call a more general function
;EXAMPLE : Refer test cases

(define (get-average lon)
  (/ (get-sum-of-non-neg-values lon)
     (length (get-actual-list lon 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-actual-list : ListOfNumber NonNegInt -> ListOfNumber
;GIVEN: A ListOfNumber lon and a NonNegInt c
;WHERE : c is a non-negative integer that toggles between 0 and 1 such that it
;        becomes 0 when a number from the list of numbers is not equal to -999
;        and becomes 1 when -999 is encountered for the first time
;RETURNS: A list of all non-negative numbers till the first -999 is 
;         encountered in the given list
;STRATEGY: Use HOF foldr on lon

(define (get-actual-list lon c)
  (foldr
   ;Number ListOfNumber -> ListOfNumber
   ;GIVEN: A number n and a list of numbers e
   ;RETURNS: A list of all non-negative numbers till the first -999 in the list
   ;STRATEGY: Cases on n
   (lambda(n e)(if (not (equal? n END-OF-DATA))
                   (get-non-neg-values e n c)
                   (get-actual-list e TRUE-FLAG)))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;get-non-neg-values : ListOfNumber Number NonNegInt -> ListOfNumber
;;GIVEN: A ListOfNumber lon and Numbers n and  c
;;WHERE: c is the non-negative integer that toggles between 0 and 1 such that
;;      it becomes 0 when a number from the list of numbers is not equal to -999
;;        and becomes 1 when the number is equal to -999
;;RETURNS: A list of all non-negative numbers till the first -999
;          is encountered
;;STRATEGY: Cases on n and c

(define (get-non-neg-values lon n c)
  (if (and (>= n ZERO) (< c TRUE-FLAG))
      (cons n lon)
      (get-actual-list lon FALSE-FLAG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-sum-of-non-neg-values : ListOfNumber -> NonNegReal
;GIVEN: A list of numbers lon 
;RETURNS: A nonnumber that represents the sum of all non-negative
;          values encountered until the first occurence of -999 in the list
;STRATEGY: Use HOF foldl on lon

(define (get-sum-of-non-neg-values lon)
  (foldl
   (lambda(n z)(+ n z))
   SUM
   (get-actual-list lon FALSE-FLAG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;TEST FOR RAINFALL FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal?
   ;test to check the average of the non-negative numbers till the first
   ;occurence of end of data value in the list
   (rainfall RAINFALL-AMT) AVG
   "Should Return the average of the non-negative numbers till the first
   occurence of -999 in the list ")
  (check-equal?
   ;test to check the average of an empty list
   (rainfall empty) 0
   "Should return the average as zero when the list is empty"))