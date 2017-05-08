;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set: 07

;; Question 1 & 2:

;; Check if the given outline is a legal flat representation
;; Convert a given outline tree representation into legal flat representation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            ;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      ;TREE REPRESENTATION OF AN OUTLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Outline is ListOfSection

(define-struct section(str secs))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Section is a (make-section String ListOfSection)

;; INTERPRETATION: (make-section str secs) is a section where
;; str is the header text of the section
;; secs is the list of subsections of the section

;; DESTRUCTOR TEMPLATE :
;;
;; section-fn : Section -> ??

;; (define (section-fn s)
;;  (...(section-str s)
;;      (section-secs s)))

;; LIST OF SECTION:

;; A ListOfSection is one of
;; -empty
;; INTERP: the outline is empty and has no sections
;; -(cons s los)
;; INTERP: (cons s los) is a sequence of subsections such that the first
;;         element s is a Section

;; TEMPLATE:
;los-fn : ListOfSection -> ??
;(define (los-fn los)
;  (cond
;    [(empty? los) empty]
;    [else (...
;           (section-fn (first los))
;           (los-fn (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ;; FLAT REPRESENTATION OF AN OUTLINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An FlatRep is a ListOfLine

(define-struct line(lon str))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Line is a (make-line ListOfNumber String)

;; INTERPRETATION: (make-line lon str)
;; lon is the section number represented as a list of natural numbers
;;   WHERE: the lon represents a subsection of a section
;;          iff the list has more than one natural number and
;;          the numbers are in order.
;; str is the header text of the section

;; DESTRUCTOR TEMPLATE :
;;
;; line-fn : Line -> ??

;; (define (line-fn l)
;;  (...(line-lon l)
;;      (line-str l)))

;; LIST OF LINE :

;; A ListOfLine is one of-
;; -empty
;; INTERP: The given outline is empty and hence the FlatRep is empty
;; -(cons l lol)
;; INTERP: (cons l lol) represents a sequence of lines in the FlatRep
;;         whose first element l is a Line.

;;TEMPLATE:
;; lol-fn : ListOfLine -> ??

; (define (lol-fn lol)
;  (cond
;    [(empty? lol) empty]
;    [else (...
;            (line-fn (first lol))
;            (lol-fn (rest lol)))]))

;; LIST OF NUMBER :

;; A ListOfNumber is one of -

;; - empty
;; Interpretation : list of numbers is empty when a given list has no numbers. 

;; - (cons n lon)
;; Interpretation : (cons n lon) represents a sequence of Numbers
;;                  whose first element n is a Number
;;                  and other numbers are represented by lon.

;; lon-fn : ListOfNumber -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;            (line-fn(first lon))
;;            (lon-fn (rest lon)))]))

;; LIST OF X :

;; A ListOfX is one of -

;; - empty
;; Interpretation : list of x is empty. 

;; - (cons x lox)
;; Interpretation : (cons x lox) represents a sequence of x's
;;                  whose first element x is a ListOfNumber 
;;                  and other x's are represented by lox.

;; lox-fn : ListOfX -> ??
;; (define (lox-fn lox)
;;   (cond
;;     [(empty? lox) ...]
;;     [else (...
;;            (lon-fn(first lox))
;;            (lox-fn (rest lox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;; CONSTANTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;TREE REPRESENTATION OF AN OUTLINE

(define TREE-REP
  (list 
   (make-section "The first section"
                 (list
                  (make-section "A subsection with no subsections" empty)
                  (make-section "Another subsection"
                                (list
                                 (make-section "This is a subsection of 1.2"
                                               empty)
                                 (make-section
                                  "This is another subsection of 1.2"
                                               empty)))
                  (make-section "The last subsection of 1" empty)))
   (make-section "Another section"
                 (list
                  (make-section "More stuff" empty)
                  (make-section "Still more stuff" empty)))))

;LEGAL FLAT REPRESENTATION OF AN OUTLINE

(define LEGAL-FLAT-REP
  (list
   (make-line (list 1) "The first section")
   (make-line (list 1 1) "A subsection with no subsections")
   (make-line (list 1 2) "Another subsection")
   (make-line (list 1 2 1) "This is a subsection of 1.2")
   (make-line (list 1 2 2) "This is another subsection of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")
   (make-line (list 2 1) "More stuff")
   (make-line (list 2 2) "Still more stuff")))

;FLAT REPRESENTATION OF AN OUTLINE THAT IS NOT LEGAL

(define FLAT-REP-NOT-LEGAL
  (list
   (make-line (list 1) "The first section")
   (make-line (list 1 1) "A subsection with no subsections")
   (make-line (list 1 2) "Another subsection")
   (make-line (list 1 1 1) "This is a subsection of 1.2")
   (make-line (list 1 2 2) "This is another subsection of 1.2")
   (make-line (list 1 3) "The last subsection of 1")
   (make-line (list 2) "Another section")
   (make-line (list 2 1) "More stuff")
   (make-line (list 2 2) "Still more stuff")))

;VALUE TO APPEND WHEN SUBSECTION IS NOT EMPTY
(define DEFAULT-NUM 1)

; INVARIANT USED
(define prev-sec empty)

; INITIAL INDEX OF A SECTION
(define INITIAL (list 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; FUNCTIONS TO CHECK IF FLAT REP IS LEGAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;legal-flat-rep? :ListOfLine -> Boolean
;GIVEN: a list of lines
;RETURNS: true iff the list is a legal flat representation of an outline.
;STRATEGY: Call a more general function
;EXAMPLE: Refer test cases

(define (legal-flat-rep? lol)
  (check-with-prev-list lol prev-sec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;check-with-prev-list : ListOfLine ListOfNumber -> Boolean
;GIVEN: A ListOfLine lol and a ListOfNumber  prev-sec
;WHERE: prev-sec is a list of natural numbers that represent the section
;       number of a section that preceeds the current section in consideration
;RETURNS: true iff a section is represented by a list of natural numbers
;         that represent the section number chronologically, when
;         compared with the list that represents the previous section number.
;STRATEGY: Use template for lol on lines
;EXAMPLE: Refer test cases

(define (check-with-prev-list lines prev-sec)
  (cond
    [(empty? lines) true]
    [else (and (check-for-all-possible-results (line-lon (first lines))
                                               prev-sec)
               (check-with-prev-list  (rest lines) (line-lon (first lines))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;check-for-all-possible-results : ListOfNumber ListOfNumber -> Boolean
;GIVEN: A ListOfNumber curr-sec and a ListOfNumber prev-sec
;WHERE: curr-sec is a list of natural numbers that represent the section
;       number of the current section in consideration,
;       and prev-sec is a list of natural numbers that represents the section
;       number of a section that preceeds the current section in consideration
;RETURNS: true iff the current section is numbered correctly with respect to
;         section preceeding it.
;STRATEGY: Cases on curr-sec and prev-sec
;EXAMPLE: Refer test cases

(define (check-for-all-possible-results curr-sec prev-sec)
  (if (equal? curr-sec (append prev-sec INITIAL))
      true
      (compare-with-all-combinations curr-sec prev-sec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;compare-with-all-combinations :ListOfNumber ListOfNumber -> Boolean
;GIVEN: A ListOfNumber curr-sec and a ListOfNumber prev-sec
;WHERE: curr-sec is a list of natural numbers that represents the
;       section number of the current section in consideration,and
;       prev-sec is a list of natural numbers that represents the section
;       number of a section that preceeds the current section in consideration
;RETURNS: true iff the current section is numbered correctly with respect to
;         section preceeding it (prev-sec).
;WHERE: the prev-sec can be a list of numbers representing a main section or
;       a list of numbers representing subsection of a section
;STRATEGY: Use HOF ormap on prev-sec
;EXAMPLE: Refer test cases

(define (compare-with-all-combinations curr-sec prev-sec)
  (ormap
   (lambda(c)(equal? c curr-sec))
   (get-all-combinations prev-sec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-all-combinations : ListOfNumber -> ListOfX
;GIVEN: A ListOfNumber prev-sec
;WHERE: prev-sec is a list of natural numbers that represents the section
;       number of a section that preceeds the current section in consideration
;RETURNS: A list containing ListOfNumber, each list representing
;         a possible section number
;WHERE: the list can be a ListOfNumber representing a main section or
;       ListOfNumber representing subsection of a section
;STRATEGY: Use template for lon on prev-sec
;EXAMPLE: Refer test cases

(define (get-all-combinations prev-sec)
  (cond
    [(empty? prev-sec) empty]
    [else (cons (append (delete-last prev-sec) (add-one-to-last prev-sec))
                (get-all-combinations (delete-last prev-sec)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;delete-last : ListOfNumber -> ListOfNumber
;GIVEN: A ListOfNumber lst
;WHERE: lst is is a list of natural numbers that represents the section
;       number of a section that preceeds the current section in consideration
;RETURNS: A ListOfNumber with last element of the list deleted
;STRATEGY: Combine simpler functions
;EXAMPLE: Refer test cases

(define (delete-last lst)
  (reverse (rest (reverse lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;add-one-to-last : ListOfNumber -> ListOfNumber
;GIVEN: A ListOfNumber lst
;WHERE: lst is is a list of natural numbers that represents the section
;       number of a section that preceeds the current section in consideration
;RETURNS: A ListOfNumber with last element of the list incremented by 1 
;STRATEGY: Combine simpler functions

(define (add-one-to-last lst)
  (list (+ (first (reverse lst)) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          ;TEST FOR LEGAL-FLAT-REP?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal?
   ;test to check if the flat representation of an outline is legal
   (legal-flat-rep? LEGAL-FLAT-REP)true
   "Should return true if the flat representation of an outline is legal")
  
  (check-equal?
   ;test to check if the flat representation of an outline is legal
   (legal-flat-rep? FLAT-REP-NOT-LEGAL)false
   "Should return false if the flat representation of an outline is not legal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; FUNCTIONS TO CONVERT TREE REP TO FLAT REP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;tree-rep-to-flat-rep : Outline -> FlatRep
;GIVEN: the representation of an outline as a list of Sections
;RETURNS: the flat representation of the outline
;STRATEGY: Call a more general function
;EXAMPLE: Refer test cases

(define (tree-rep-to-flat-rep los)
  (subtree-to-flat-rep los empty 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;subtree-to-flat-rep : ListOfSection ListOfNumber NonNegInt -> FlatRep
;GIVEN: A ListOfSection 
;       a ListOfNumber lon and
;       a non negative integer num
;WHERE : lon is a list of numbers representing the section numbers
;        num is the number that is the last index of each new subsection of
;        the parent section
;RETURNS: A list of lines representing each section along with their
;         section numbers displayed as a ordered list of natural numbers
;STRATEGY: Use template for los on sec
;EXAMPLE: Refer test cases

(define (subtree-to-flat-rep sec lon num)
  (cond
    [(empty? sec) empty]
    [else (append (sections-to-lines (first sec) lon num)
                  (subtree-to-flat-rep (rest sec) lon (+ num 1)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;sections-to-lines : Section ListOfNumber NonNegInt -> ListOfLine
;GIVEN: A section s, a ListOfNumber lon and a NonNegInt num
;WHERE: lon is a list of numbers representing the section number of a section
;       and num is the number that is the last index of each new subsection of
;       the parent section
;RETURNS: A list of lines representing each section s along with their
;         section numbers displayed as a ordered list of natural numbers
;STRATEGY: Use template for Section on s
;EXAMPLE: Refer test cases

(define (sections-to-lines s lon num)
  (cons (get-line-from-section s lon num)
        (subtree-to-flat-rep (section-secs s)
                             (get-new-index lon num)
                             (check-subsections-present (section-secs s) num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-line-from-section: Section ListOfNumber NonNegInt -> Line
;GIVEN: A section s, a ListOfNumber lon and a NonNegInt num
;WHERE: lon is a list of numbers representing the section number
;       and num is the number that is the last index of each new subsection of
;       the parent section
;RETURNS: A line representing each section s as a line
;         along with its section number displayed as a list of natural numbers
;STRATEGY: Use template for Section on s
;EXAMPLE: Refer test cases

(define (get-line-from-section s lon num)
  (make-line (get-new-index lon num) (section-str s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-new-index : ListOfNumber NonNegInt -> ListOfNumber
;GIVEN: A list of numbers lon and a non negative integer num
;WHERE: lon is a list of numbers representing the section number of a section
;       and num is the number that is the last index of each new subsection
;       of the parent section
;RETURNS: A new list of natural numbers representing the section
;        number of each new section 
;STRATEGY: Combine Simpler Functions
;EXAMPLE: Refer test cases

(define (get-new-index lon num)
  (append lon (list num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;check-subsections-present : ListOfSection NonNegInt -> NonNegInt
;GIVEN: A list of sections sub-sec which contains the subsections
;       of a section and
;       a non negative integer n
;WHERE : n is the number that is the last index
;        of each new subsection of the parent section
;RETURNS: a non negative integer n which is the number that is the last index
;         of a new subsection iff the given section has subsections
;STRATEGY: Cases on sub-sec
;EXAMPLE: Refer test cases

(define (check-subsections-present sub-sec n)
  (if (empty? sub-sec)
      n
      DEFAULT-NUM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;TEST FOR TREE-REP-TO-FLAT-REP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal?
   ;test to check if the given tree representation of an outline is
   ;converted to a legal flat representation
   (tree-rep-to-flat-rep TREE-REP) LEGAL-FLAT-REP
   "Should convert the given tree representation to flat representation"))