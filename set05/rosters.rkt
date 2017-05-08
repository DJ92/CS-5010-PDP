;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Problem Set: 05

;; Question 3:
;;
;; The Registrar has heard about your excellent work with the absent-minded
;; professors and so he asks you to solve the following problem:
;; You are given a list of (student, class) pairs. Deliver a file called
;; rosters.rkt that produces the class roster for each class that has at
;; least one student enrolled. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)

(require "extras.rkt")
(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS :

;; NONE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ENROLLMENT : 

(define-struct enrollment(student class))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Enrollment is a (make-enrollment student class)

;; INTERPRETATIONS :
;;  - student is the name of the Student.
;;  - class is the class to which the student belongs.

;; DESTRUCTOR TEMPLATE :
;;
;; enrollment-fn : String String -> ??

;; (define (enrollment-fn e)
;;  (...(enrollment-student e)
;;      (enrollment-class e)))

;; CLASS ROSTER : 

(define-struct roster(classname students))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A classnameRoster is a (make-roster class students)

;; INTERPRETATIONS :
;;  - classname is the class to which the Student belongs.
;;  - students is the SetOfStudents.

;; DESTRUCTOR TEMPLATE :
;;
;; roster-fn : String SetOfStudents -> ??

;; (define (roster-fn r)
;;  (...(roster-classname r)
;;      (roster-students r)))

;; LIST OF STUDENTS :

;; A ListOfStudents is one of -

;; - empty
;; Interpretation : a sequence of Students with no elements

;; - (cons s los)
;; Interpretation : (cons s los) represents a sequence of Student's
;;                  whose first element is s
;;                  and other elements are represented by los.

;; los-fn : ListOfStudents -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los) ...]
;;     [else (...
;;             (enrollment-fn (first los))
;;             (los-fn (rest los)))]))

;; A SetOfStudents is a ListOfStudents with no duplicates

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; roster=? : ClassRoster ClassRoster -> Boolean
;; GIVEN : two class rosters cr1 & cr2
;; RETURNS : true iff the two class rosters' cr1 & cr2 are exactly same.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for ClassRoster on cr1,cr2

(define (roster=? cr1 cr2)
  (and
   (equal? (roster-classname cr1) (roster-classname cr2))
   (set-equal? (roster-students cr1) (roster-students cr2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-true (roster=? (make-roster "PDP" (list "D" "J"))
                          (make-roster "PDP" (list "D" "J")))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; subset? : SetOfStudents SetOfStudents -> Boolean
;; GIVEN : two SetOfStudents sos1 & sos2
;; RETURNS : true iff at least every item in the first set sos1 is present in the
;;           second set sos2 of students.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF andmap on sos1

(define (subset? sos1 sos2)
  (andmap
   ; Student -> Boolean
   ; GIVEN : a student from the sos1
   ; RETURNS : true iff the student is present in sos2
   (lambda (stud) (set-member? stud sos2))
   sos1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; set-equal? : SetOfStudents SetOfStudents -> Boolean
;; GIVEN : two SetOfStudents sos1 & sos2
;; RETURNS : true iff every item in the first set sos1 is present in the second
;;           set sos2 of students and vice versa.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (set-equal? sos1 sos2)
  (and
   (subset? sos1 sos2)
   (subset? sos2 sos1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; set-member? : Student SetOfStudents -> Boolean
;; GIVEN : a Student x and a corresponding SetOfStudents sos2
;; RETURNS : true iff the Student s belongs to the SetOfStudents sos2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF ormap on sos2

(define (set-member? x sos2)
  (ormap
   ; Student -> Boolean
   ; GIVEN : a student from the sos2
   ; WHERE : x is a student of sos1
   ; RETURNS : true iff the given student stud is equal to x
   (lambda (stud) (equal? x stud))
   sos2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN : two sets of class rosters' scr1 & scr2
;; RETURNS : true iff the two sets scr1 & scr2 of class rosters' are exactly same.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (rosterset=? scr1 scr2)
   (set-roster-equal? scr1 scr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-true (rosterset=? (list (make-roster "PDP" (list "D" "J"))
                                   (make-roster "PDP" (list "D" "J")))
                             (list (make-roster "PDP" (list "D" "J"))
                                   (make-roster "PDP" (list "D" "J"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; set-roster-member? : ClassRoster SetOfClassRosters -> Boolean 
;; GIVEN : a ClassRoster x and a SetOfClassRosters scr1
;; RETURNS : true iff the class roster x belongs to the set of class rosters scr1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF ormap on scr1

(define (set-roster-member? x scr1)
  (ormap
   ; ClassRoster -> Boolean
   ; GIVEN : a roster rost from the scr1
   ; WHERE : x is a roster from the scr2
   ; RETURNS : true iff the given roster rost is equal to x
   ;           i.e both rosters are exactly the same.
   (lambda (rost) (roster=? x rost))
   scr1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; subset-roster? : SetOfClassRosters SetOfClassRosters -> Boolean
;; GIVEN : two SetOfClassRosters scr1 & scr2
;; RETURNS : true iff the first set scr1 is a subset of the second set scr2
;;           of class rosters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF andmap on scr1

(define (subset-roster? scr1 scr2)
  (andmap
   ; ClassRoster -> Boolean
   ; GIVEN : a roster rost from the scr1
   ; RETURNS : true iff the given roster rost is present in scr2
   ;           i.e given roster rost is a part of scr2
   (lambda (rost) (set-roster-member? rost scr2))
   scr1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; set-roster-equal? : SetOfClassRosters SetOfClassRosters -> Boolean
;; GIVEN : two SetOfClassRosters scr1 & scr2
;; RETURNS : true iff the first set scr1
;;           is equal to the second set scr2 of class rosters.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (set-roster-equal? scr1 scr2)
  (and
   (subset-roster? scr1 scr2)
   (subset-roster? scr2 scr1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; enrollments-to-rosters : SetOfEnrollment -> SetOfClassRoster
;; GIVEN : one set of enrollments soe
;; RETURNS :the set of class rosters for the given enrollments soe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF map on soe

(define (enrollments-to-rosters soe)
   (remove-dup (map
                ; Enrollment -> Roster
                ; GIVEN : an enrollment from the set soe
                ; RETURNS : a roster with the enrollment added to it.
                (lambda(en)(make-roster
                            (get-class en)
                            (get-roster-with-student-list en soe)))
                soe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check rosterset=? (enrollments-to-rosters
                 (list (make-enrollment "John" "PDP")
                       (make-enrollment "Kathryn" "Networks")
                       (make-enrollment "Feng" "PDP")
                       (make-enrollment "Amy" "PDP")
                       (make-enrollment "Amy" "Networks")))
                (list
                 (make-roster "PDP" (list "John" "Feng" "Amy"))
                 (make-roster "Networks" (list "Kathryn" "Amy")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; get-roster-with-student-list : Enrollment SetOfEnrollments -> SetOfStudents
;; GIVEN : one enrollment en1 and a set of enrollments soe
;; RETURNS : the set of students for the given enrollment class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF filter on soe

(define (get-roster-with-student-list en1 soe)
  (get-student-list (filter
                     ; Enrollment -> Boolean
                     ; GIVEN : an enrollment from the list
                     ; WHERE : en1 is another enrollment from the list
                     ; RETURNS : true iff the class of both enrollments are
                     ;           same.
                     (lambda(en)(equal? (get-class en) (get-class en1)))
                     soe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; get-class : Enrollment -> String
;; GIVEN : one enrollment en
;; RETURNS : the corresponding class of that enrollment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use Template for Enrollment on en

(define (get-class en)(enrollment-class en))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; get-student-list : SetOfEnrollments -> SetOfStudents
;; GIVEN : a set of enrollments soe
;; RETURNS : the corresponding set of students for a similar class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF map on soe

(define (get-student-list soe)
  (map
   ; Enrollment -> Student
   ; GIVEN : an enrollment en from the list
   ; WHERE : the class for all enrollments is the same
   ; RETURNS : the student enrolled for that particular class
   (lambda(en)(enrollment-student en))
   soe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; remove-dup : ListOfRosters -> SetOfRosters
;; GIVEN: a list of rosters lor
;; RETURNS: a set of rosters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (remove-dup (list (make-roster "PDP" (list "D" "J")))
;;                   (make-roster "NS" (list "S" "N"))
;;                   (make-roster "PDP" (list "D" "J"))))

;; (list (make-roster "PDP" (list "D" "J")))
;;       (make-roster "NS" (list "S" "N")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF foldr on sor

(define (remove-dup lor)
  (foldr
   ; Roster EmptyList -> SetOfRoster 
   ; GIVEN : a Roster r and an empty list e
   ; RETURNS : a set of rosters
   (lambda(r e)(non-dup-fn r e))
   empty
   lor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (remove-dup (list
                             (make-roster "PDP" (list "John" "Feng" "Amy"))
                             (make-roster "Networks" (list "Kathryn" "Amy"))
                             (make-roster "PDP" (list "John" "Feng" "Amy"))
                             (make-roster "Networks" (list "Kathryn" "Amy"))))
                (list
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; non-dup-fn : Roster ListOfRosters -> SetOfRosters
;; GIVEN: a Roster r and a ListOfRosters lor
;; RETURNS: a SetOfRosters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

;; (non-dup-fn (make-roster "PDP" (list "D" "J" "S" "N")) list((make-roster
;;                                               "PDP" (list "D" "J" "S" "N"))))
;; (list (make-roster "PDP" (list "D" "J" "S" "N")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (non-dup-fn r lor)
    (if (is-a-dup? r lor)
        lor
        (cons r lor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; is-a-dup? : Roster ListOfRosters -> Boolean
;; GIVEN: a Roster r1 and a ListOfRosters lor
;; RETURNS: true iff the roster r1 is present in the list of rosters lor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Use HOF ormap on lor

(define (is-a-dup? r1 lor)
  (ormap
   ; Roster -> Boolean
   ; GIVEN : a roster r2 from the list lor
   ; WHERE : r1 is the roster from another list
   ; RETRUNS : true iff the two rosters r1 & r2 are same.
   (lambda(r2)(check-dup-fn r1 r2))
   lor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :
;;
;; check-dup-fn? : Roster Roster -> Boolean
;; GIVEN: two Rosters r1 & r2
;; RETURNS: true iff the two rosters match

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STRATEGY : Combining Simpler Functions

(define (check-dup-fn r1 r2)
  (roster=? r1 r2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASE:

(begin-for-test
  (check-equal? (check-dup-fn (make-roster "PDP" (list "D" "J"))
                              (make-roster "PDP" (list "D" "J"))) true))