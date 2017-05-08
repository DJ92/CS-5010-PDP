;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 05

;; Question 1:
;;
;; (The absent-minded professors, part 2).
;; Reimplement the last problem from last week's problem set (class-lists.rkt),
;; but using HOFs wherever possible and appropriate. Be sure to provide
;; make-slip, slip-color, slip-name1 and slip-name2, which I forgot to specify
;; in PS04. Use the filename class-lists.rkt, as you did before.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(require "extras.rkt")
(provide
 felleisen-roster
 shivers-roster
 make-slip
 slip-color
 slip-name1
 slip-name2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; PROFESSOR FELLEISEN SLIP COLOR

(define YELLOW-COLOR "yellow")

;; PROESSOR SHIVERS SLIP COLOR

(define BLUE-COLOR "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SLIP : 

(define-struct slip(color name1 name2))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Slip is (make-slip Color String String)

;; INTERPRETATIONS :
;;  - color is the color of the slip
;;  - name1 is the first part of of the student's name
;;  - name2 is the last parr of the student's name

;; DESTRUCTOR TEMPLATE :
;;
;; slip-fn : Slip -> ??

;; (define (slip-fn s)
;;  (...(slip-color s)
;;      (slip-name1 s)
;;      (slip-name2 s)))

;; LIST OF SLIPS :

;; A ListOfSlips is one of -

;; - empty
;; Interpretation : a sequence of Slips with no elements

;; - (cons s los)
;; Interpretation : (cons s los) represents a sequence of Slip's
;;                  whose first element is s
;;                  and other elements are represented by los.

;; los-fn : ListOfSlips -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (slip-fn (first los))
;;             (los-fn (rest los)))]))

;; COLOR :

;; A Color is one of
;; -- "yellow" 
;; -- "blue"
;; Interpretation: self-evident

;; TEMPLATE
;; color-fn : Color -> ??
;; (define (color-fn c)
;;  (cond
;;    [(string=? c "blue")    
;;     ...]
;;    [(string=? c "yellow")
;;     ...]))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MIXED LIST

;; An MixedList is a ListOfSlips.
;; Interp: the list of students of both Professor Felleisen's and
;;         Prof. Shivers' students.

(define ml
  (list
    (make-slip "yellow" "D" "J")
    (make-slip "blue" "N" "S")
    (make-slip "yellow" "J" "D")
    (make-slip "yellow" "J" "D")
    (make-slip "yellow" "D" "J")
    (make-slip "blue" "S" "N")))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; felleisen-roster : ListOfSlips -> ListOfSlips
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Felleisen's class, without duplication.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (felleisen-roster (cons (make-slip "yellow" "D" "J")
;;                                 (cons (make-slip "yellow" "J" "D") '())))
;; (cons (make-slip "yellow" "D" "J") '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF filter on los

(define (felleisen-roster los)
  (remove-dup
   (filter
    ; X -> Boolean
    ; GIVEN : a slip from the list
    ; RETURNS: true iff the slip belongs to the felleisen roster
    ;          (i.e the slip color is yellow)
    (lambda(s)(slip-belongs-to-felleisen-roster? s))
    los)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 1:

(begin-for-test
  (check-equal? (felleisen-roster ml) (cons (make-slip "yellow" "D" "J") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; slip-belongs-to-felleisen-roster : Slip -> Boolean
;; GIVEN: a slip s
;; RETURNS: true iff the slip belongs to felleisen's list of slips
;;          i.e slip color is yellow

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (slip-belongs-to-felleisen-roster? (make-slip "yellow" "D" "J"))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template Slip on s

(define (slip-belongs-to-felleisen-roster? s)
  (if (string=? (slip-color s) YELLOW-COLOR)
      true
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 2 & 3:

(begin-for-test
  (check-equal? (slip-belongs-to-felleisen-roster? (make-slip "yellow" "D" "J"))
                true)
  (check-equal? (slip-belongs-to-felleisen-roster? (make-slip "blue" "D" "J"))
                false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; shivers-roster: ListOfSlips -> SetOfSlips
;; GIVEN: a list of slips los
;; RETURNS: a set of slips

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (shivers-roster (cons (make-slip "blue" "D" "J")
;;                                 (cons (make-slip "blue" "J" "D") '())))
;; (cons (make-slip "blue" "D" "J") '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF filter on los

(define (shivers-roster los)
  (remove-dup
   (filter
    ; Slip -> Boolean
    ; GIVEN : a slip from the list
    ; RETURNS: true iff the slip belongs to the shivers roster
    ;          (i.e the slip color is blue)
    (lambda(s)(slip-belongs-to-shivers-roster? s))
    los)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 4:

(begin-for-test
  (check-equal? (shivers-roster ml) (cons (make-slip "blue" "S" "N") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; slip-belongs-to-shivers-roster : Slip -> Boolean
;; GIVEN: a slip
;; RETURNS: true iff the slip belongs to shivers' list of slips
;;          i.e slip color is blue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (slip-belongs-to-shivers-roster? (make-slip "blue" "D" "J"))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template Slip on s

(define (slip-belongs-to-shivers-roster? s)
  (if (string=? (slip-color s) BLUE-COLOR)
      true
      false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 5 & 6:

(begin-for-test
  (check-equal? (slip-belongs-to-shivers-roster? (make-slip "blue" "D" "J"))
                true)
  (check-equal? (slip-belongs-to-shivers-roster? (make-slip "yellow" "D" "J"))
                false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; remove-dup : ListOfSlips -> SetOfSlips
;; GIVEN: a list of slips los
;; RETURNS: a SetOfSlips

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (remove-dup (cons (make-slip "blue" "D" "J")
;;             (cons (make-slip "blue" "J" "D") '()))

;; (cons (make-slip "blue" "J" "D") '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF foldr on los

(define (remove-dup los)
  (foldr
   ; Slip EmptyList -> SetOfSlips
   ; GIVEN: a slip s from the list los and an empty list e
   ; RETURNS: a set of slips
   (lambda(s e)(non-dup-fn s e))
   empty
   los))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; non-dup-fn : Slip ListOfSlips -> SetOfSlips
;; GIVEN: a Slip s and a ListOfSlips los
;; RETURNS: a SetOfSlips

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (non-dup-fn (make-slip "blue" "D" "J") '()))
;; (list (make-slip "blue" "D" "J"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (non-dup-fn s los)
    (if (is-a-dup? s los)
        los
        (cons s los)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 7:

(begin-for-test
  (check-equal? (remove-dup (cons (make-slip "yellow" "D" "J")
                                  (cons (make-slip "yellow" "J" "D") '())))
                (cons (make-slip "yellow" "J" "D") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; is-a-dup? : Slip ListOfSlips -> ListOfSlips
;; GIVEN: a Slip and a ListOfSlips
;; RETURNS: true iff the slip is present in the list of slips.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF ormap on los

(define (is-a-dup? slp1 los)
  (ormap
   ; Slip -> Boolean
   ; GIVEN : a slip slp2 from a list los
   ; WHERE : slip slp1 is from another list
   ; RETURNS : true iff the slip slp2 is equal to slp1
   (lambda(slp2)(check-dup-fn slp1 slp2))
   los))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; check-dup-fn? : Slip Slip -> Boolean
;; GIVEN: two Slips slp1 & slp2
;; RETURNS: true iff the two slips match

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template of Slip on slp1,slp2

(define (check-dup-fn slp1 slp2)
  (or
   (and
    (match? (slip-name1 slp1) (slip-name1 slp2))
    (match? (slip-name1 slp1) (slip-name1 slp2)))
   (and
    (match? (slip-name1 slp1) (slip-name2 slp2))
    (match? (slip-name2 slp1) (slip-name1 slp2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; match? : String String -> Boolean
;; GIVEN: two Strings
;; RETURNS: true iff the two strings are equal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (match? "Dheeraj" "Dheeraj")
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (match? str1 str2)
  (string=? str1 str2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (match? "D" "D") true))
