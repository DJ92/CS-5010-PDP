;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 04

;; Question 3:
;;
;; Professor Felleisen and Professor Shivers each keep their class lists on
;; slips of paper,one student on each slip. Professor Felleisen keeps his list
;; on slips of yellow paper.

;; Professor Shivers keeps his list on slips of blue paper.
;; Unfortunately, both professors are sloppy record-keepers. Sometimes they have
;; more than one slip for the same student. Sometimes they record the student
;; names first-name first sometimes they record the names last-name first.

;; One day, Professor Felleisen was walking up the stairs in WVH, talking to one
;; of his graduate students. At the same time, Professor Shivers was walking
;; down the stairs, all the time talking to one of his graduate students. They
;; collided, and dropped all the slips containing their class lists on the
;; stairs, where they got all mixed up.

;; Your job is to clean up this mess. Deliver a file named class-lists.rkt

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
 slip-color
 slip-fname
 slip-lname)

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

(define-struct slip(color fname lname))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Slip is (make-slip Color String String)

;; INTERPRETATIONS :
;;  - color is the color of the slip
;;  - fname is the first part of of the student's name
;;  - lname is the last parr of the student's name

;; DESTRUCTOR TEMPLATE :
;;
;; slip-fn : Slip -> ??

;; (define (slip-fn s)
;;  (...(slip-color s)
;;      (slip-fname s)
;;      (slip-lname s)))

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

;; STRATEGY : Use Template for ListOfSlips on los

(define (felleisen-roster los)
  (cond[(empty? los) empty]
       [else (remove-dup
              (if (not (slip-belongs-to-felleisen-roster? (first los)))
                 (felleisen-roster (rest los))
                 (cons (first los) (felleisen-roster (rest los)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 1:

(begin-for-test
  (check-equal? (felleisen-roster ml) (cons (make-slip "yellow" "D" "J") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; slip-belongs-to-felleisen-roster : Slip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff the slip belongs to felleisen's list of slips

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (slip-belongs-to-felleisen-roster? (make-slip "yellow" "D" "J"))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combine Simpler Functions

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
;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Shivers' class, without duplication.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (shivers-roster (cons (make-slip "blue" "D" "J")
;;                                 (cons (make-slip "blue" "J" "D") '())))
;; (cons (make-slip "blue" "D" "J") '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (shivers-roster los)
  (cond[(empty? los) empty]
       [else (remove-dup
              (if (not (slip-belongs-to-shivers-roster? (first los)))
                 (shivers-roster (rest los))
                 (cons (first los) (shivers-roster (rest los)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 4:

(begin-for-test
  (check-equal? (shivers-roster ml) (cons (make-slip "blue" "S" "N") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; slip-belongs-to-shivers-roster : Slip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff the slip belongs to shivers' list of slips

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (slip-belongs-to-shivers-roster? (make-slip "blue" "D" "J"))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

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
;; remove-dup : ListOfSlips -> ListOfSlips
;; GIVEN: a list of slips
;; RETURNS: a ListOfSlips with no duplicates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (remove-dup (cons (make-slip "blue" "D" "J")
;;             (cons (make-slip "blue" "J" "D") '()))

;; (cons (make-slip "blue" "J" "D") '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (remove-dup los)
  (cond[(empty? los) empty]
       [else (if
              (or
               (and (contains-fname-in-fname-list? (slip-fname (first los))
                                                   (rest los))
                    (contains-lname-in-lname-list? (slip-lname (first los))
                                                   (rest los)))
               (and (contains-fname-in-lname-list? (slip-fname (first los)) los)
                    (contains-lname-in-fname-list? (slip-lname (first los)) los)))
              (remove-dup (rest los))
              (cons (first los) (remove-dup (rest los))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 7:

(begin-for-test
  (check-equal? (remove-dup (cons (make-slip "yellow" "D" "J")
                                  (cons (make-slip "yellow" "J" "D") '())))
                (cons (make-slip "yellow" "J" "D") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; contains-fname-in-fname-list? : String ListOfSlips -> Boolean
;; GIVEN: a String and a ListOfSlips
;; RETURNS: true iff the string is present in the fname field of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (contains-fname-in-fname? "D" (cons (make-slip "blue" "D" "J") '()))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (contains-fname-in-fname-list? str los)
  (cond[(empty? los) false]
       [else (if (string=? str (slip-fname (first los)))
                 true
                 (contains-fname-in-fname-list? str (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 8:

(begin-for-test
  (check-equal? (contains-fname-in-fname-list? "D" (cons (make-slip
                                                          "yellow"
                                                          "D" "J")
                                                   (cons
                                                         (make-slip
                                                          "yellow"
                                                          "J" "D") '())))
                true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; contains-lname-in-lname-list? : String ListOfSlips -> Boolean
;; GIVEN: a String and a ListOfSlips
;; RETURNS: true iff the string is present in the lname field of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (contains-lname-in-lname? "J" (cons (make-slip "blue" "D" "J") '()))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (contains-lname-in-lname-list? str los)
  (cond[(empty? los) false]
       [else (if (string=? str (slip-lname (first los)))
                 true
                 (contains-lname-in-lname-list? str (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 9 & 10:

(begin-for-test
  (check-equal? (contains-lname-in-lname-list? "D" (cons (make-slip
                                                          "yellow"
                                                          "D" "J")
                                                   (cons
                                                         (make-slip
                                                          "yellow"
                                                          "J" "D") '())))
                true)
  (check-equal? (contains-lname-in-lname-list? "D" empty) false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; contains-fname-in-lname-list? : String ListOfSlips -> Boolean
;; GIVEN: a String and a ListOfSlips
;; RETURNS: true iff the string is present in the lname field of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (contains-fname-in-lname? "D" (cons (make-slip "blue" "J" "D") '()))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (contains-fname-in-lname-list? str los)
  (cond[(empty? los) false]
       [else (if (string=? str (slip-lname (first los)))
                 true
                 (contains-fname-in-lname-list? str (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 11:

(begin-for-test
  (check-equal? (contains-fname-in-lname-list? "D" (cons (make-slip
                                                          "yellow"
                                                          "D" "J")
                                                   (cons
                                                         (make-slip
                                                          "yellow"
                                                          "J" "D") '())))
                true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; contains-lname-in-fname-list? : String ListOfSlips -> Boolean
;; GIVEN: a String and a ListOfSlips
;; RETURNS: true iff the string is present in the lname field of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (contains-lname-in-fname? "J" (cons (make-slip "blue" "D" "J") '()))
;; #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ListOfSlips on los

(define (contains-lname-in-fname-list? str los)
  (cond[(empty? los) false]
       [else (if (string=? str (slip-fname (first los)))
                 true
                 (contains-lname-in-fname-list? str (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES 12 & 13:

(begin-for-test
  (check-equal? (contains-lname-in-fname-list? "D" (cons (make-slip
                                                          "yellow"
                                                          "D" "J")
                                                   (cons
                                                         (make-slip
                                                          "yellow"
                                                          "J" "D") '())))
                true)
  (check-equal? (contains-lname-in-fname-list? "D" empty) false))
