;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 2: Exercise 111
;;
;; You are to design a set of functions that illustrate the workings of a
;; finite-state machine for accepting strings that exactly match the regular expression

;; (a | b)* c (a | b)* d (e | f)*

;; So cd, abcd, abcbdef and aacbadf all match, but abc, abdbcef, and acbded do not.
;; The legal inputs to the machine are precisely the strings "a", "b", "c", "d", "e", and "f".
;; Any other inputs violate the machine's contract, and its behavior on those inputs is unspecified.

;; You will need to provide data definitions for State and for MachineInput.
;; Be sure to write an interpretation for each state. There is no need to write an interpretation
;; for MachineInput, since the problem is already phrased in terms of strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")
(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; STATE :

(define (state str)
  (cond[(number? str) "Initial State"]
       [(string=? str "Initial State") "Initial State"]
       [(string=? str "Intermediate State") "Intermediate State"]
       [(string=? str "Final State") "Final State"]
       [(string=? str "Error State") "Error State"]))

;; a State is one of
;; -- "Initial State"
;; -- "Intermediate State"
;; -- "Final State"
;; -- "Error State"

;; INTERPRETATION: self-evident

;; MACHINE INPUT :

(define machine-input "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;;          of your machine.  The given number is ignored.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (initial-state num)(state num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (next-state machine-input state)
  (cond[(string=? state "Initial State")
        (cond[(string=? machine-input "a") "Initial State"]
             [(string=? machine-input "b") "Initial State"]
             [(string=? machine-input "c") "Intermediate State"]
             [else "Error State"])]
       [(string=? state "Intermediate State")
        (cond[(string=? machine-input "a") "Intermediate State"]
             [(string=? machine-input "b") "Intermediate State"]
             [(string=? machine-input "d") "Final State"]
             [else "Error State"])]
       [(string=? state "Final State")
        (cond[(string=? machine-input "e") "Final State"]
             [(string=? machine-input "f") "Final State"]
             [else "Error State"])]
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (accepting-state? state)
  (cond[(string=? state "Final State") true]
       [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;;          state to an accepting state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (error-state? state)
  (cond[(string=? state "Error State") true]
       [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (initial-state 2)
;  "Initial State"

;; (next-state "b" (initial-state 2))
;  "Initial State"

;; (accepting-state? (next-state "e" (initial-state 2)))
;  #false

;; (accepting-state? (next-state "d" "Intermediate State"))
;  #true

;; (error-state? (next-state "d" "Intermediate State"))
;  #false

;; (error-state? (next-state "c" "Intermediate State"))
;  #true

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test
  (check-equal? (initial-state 2) "Initial State"))

(begin-for-test
  (check-equal? (next-state "b" (initial-state 2)) "Initial State"))

(begin-for-test
  (check-equal? (next-state "d" (next-state "c" (initial-state 2))) "Final State"))

(begin-for-test
  (check-equal? (accepting-state? (next-state "e" (initial-state 2))) false))

(begin-for-test
  (check-equal? (accepting-state? (next-state "d" "Intermediate State")) true))

(begin-for-test
  (check-equal? (error-state? (next-state "d" "Intermediate State")) false))

(begin-for-test
  (check-equal? (error-state? (next-state "c" "Intermediate State")) true))
