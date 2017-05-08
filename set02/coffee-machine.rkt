;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 3:
;;
;; A coffee machine has two items: coffee and hot chocolate.
;; Coffee is $1.50, but hot chocolate is $0.60. A customer may put any sequence of coins into the machine,
;; and then select an item. If the customer has deposited enough money into the machine, and the machine is
;; not out of the selected item, then the machine will dispense the requested item. If the machine is
;; out of the selected item, the machine will flash "Out of Item".
;; The customer may also press "change", in which case the machine will return any unspent money that the customer
;; has put in during the current transaction. If none of these apply, the machine does nothing.
;; For example, the customer may put three 25-cent pieces into the machine. If he then selects the hot chocolate,
;; the machine will dispense a cup of hot chocolate. If he tries to select the coffee instead, nothing will happen.
;; If the customer then presses "change", the machine will return the extra $0.15 that he is owed.
;; The customer may request "change" at any time, whether or not he has ordered anything, and we assume
;; that the machine can always make the required amount of change.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")
(provide
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-coffee
 machine-remaining-chocolate
 machine-bank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; CUSTOMER INPUT:

(define (cust-input input)
 (cond
   [(number? input) input]
   [(string=? input "coffee") "coffee"]
   [(string=? input "hot chocolate")  "hot chocolate"]
   [(string=? input "change") "change"]))

;; a Customer Input (cust-input) is one of
;; -- amount
;; -- "coffee" 
;; -- "hot chocolate"
;; -- "change"

;; INTERPRETATION: self-evident

;; MACHINE STATE
(define-struct machine-state(cnt_cof cnt_hot bank user_credit))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Machine State is (make-machine-state NonNegInt NonNegInt NonNegInt)

;; INTERPRETATIONS :
;;  - cnt_cof is the number of coffees in stock
;;  - cnt_hot is the number of hot chocolates in stock
;;  - bank is the amount of money in the machine
;;  - user_credit is the amount entered by the user

;; DESTRUCTOR TEMPLATE :
;;
;; machine-state-fn : NonNegInt NonNegInt NonNegInt -> ??

;; (define (machine-state-fn ms)
;;  (...(machine-state-cnt_cof machine-state)
;;      (machine-state-cnt_hot machine-state)
;;      (machine-state-bank machine-state)
;;      (machine-state-user_credit machine-state)))

;; MACHINE OUTPUT

(define (machine-out output)
  (cond
    [(string=? output "coffee") "coffee"]
    [(string=? output "hot-chocolate") "hot chocolate"]
    [(string=? output "Out of Item") "Out of Item"]
    [(number? output) output]
    [(string=? output "Nothing") ""]))

;; a Machine Output (machine-out) is one of
;; -- "coffee"
;; -- "hot chocolate" 
;; -- "Out of Item"
;; -- remaining change as number
;; -- Nothing
;; INTERPRETATION: self-evident

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of cups of coffee and of hot chocolate
;; RETURNS: the state of a machine loaded with the given number of cups
;;          of coffee and of hot chocolate, with an empty bank.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use MachineState Template
(define (initial-machine cnt_cof cnt_hot bank)
  (make-machine-state cnt_cof cnt_hot bank 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;;          input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use MachineState Template and CustomerInput

(define (machine-next-state ms ci)
  (cond [(number? ci) (make-machine-state (machine-state-cnt_cof ms) (machine-state-cnt_hot ms) (machine-state-bank ms) (+ (machine-state-user_credit ms) ci))]
        [(and (>= (machine-state-user_credit ms) 150) (string=? ci "coffee")) (if (> (machine-state-cnt_cof ms) 0) (make-machine-state (- (machine-state-cnt_cof ms) 1) (machine-state-cnt_hot ms) 150 (- (machine-state-user_credit ms) 150)) "Out of Item")]
        [(and (>= (machine-state-user_credit ms) 60) (string=? ci "hot chocolate")) (if (> (machine-state-cnt_hot ms) 0) (make-machine-state (machine-state-cnt_cof ms) (- (machine-state-cnt_hot ms) 1) 60 (- (machine-state-user_credit ms) 60)) "Out of Item")] 
        [(and (string=? ci "change") (> (machine-state-user_credit ms) 0)) (make-machine-state (machine-state-cnt_cof ms) (machine-state-cnt_hot ms) (machine-state-bank ms) (- (machine-state-user_credit ms) (machine-state-user_credit ms)))]
        [else "Nothing"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;;          customer input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use MachineState Template and CustomerInput

(define (machine-output ms ci)
  (cond[(and (number? ci) (> ci 0)) "Nothing"]
       [(and (string=? ci "coffee") (>= (machine-state-user_credit ms) 150)) (if (> (machine-state-cnt_cof ms) 0) "coffee" "Out of Item")]
       [(and (string=? ci "hot chocolate") (>= (machine-state-user_credit ms) 60)) (if (> (machine-state-cnt_hot ms) 0) "hot chocolate" "Out of Item")]
       [(and (string=? ci "change") (> (machine-state-user_credit ms) 0)) (machine-state-user_credit ms)]
       [else "Nothing"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of coffee left in the machine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Machine State Template
(define (machine-remaining-coffee ms)(machine-state-cnt_cof ms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of hot chocolate left in the machine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Machine State Template
(define (machine-remaining-chocolate ms)(machine-state-cnt_hot ms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Machine State Template
(define (machine-bank ms)(machine-state-bank ms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (initial-machine 10 20 0)
;  (make-machine-state 10 20 0 0)

;; (machine-next-state (initial-machine 10 20 0) 100)
;  (make-machine-state 10 20 0 100)

;; (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate")
;  (make-machine-state 10 19 60 40)

;; (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate") "change")
;  (make-machine-state 10 19 60 0)

;; (machine-output (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate") "change") 456)
;  "Nothing"

;; (machine-output (machine-next-state (machine-next-state (initial-machine 10 20 0) 140) 60) "coffee")
;  "coffee"

;; (machine-output (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 140) 60) "coffee") "change")
;  50

;; (machine-output (machine-next-state (machine-next-state (initial-machine 0 20 0) 140) 60) "coffee")
;  "Out of Item"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test
  (check-equal? (initial-machine 10 20 0)  (make-machine-state 10 20 0 0)))

(begin-for-test
  (check-equal? (machine-next-state (initial-machine 10 20 0) 100) (make-machine-state 10 20 0 100)))

(begin-for-test
  (check-equal? (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate") (make-machine-state 10 19 60 40)))

(begin-for-test
  (check-equal? (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate") "change") (make-machine-state 10 19 60 0)))

(begin-for-test
  (check-equal? (machine-output (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 100) "hot chocolate") "change") 456) "Nothing"))

(begin-for-test
  (check-equal? (machine-output (machine-next-state (machine-next-state (initial-machine 10 20 0) 140) 60) "coffee") "coffee"))

(begin-for-test
  (check-equal? (machine-output (machine-next-state (machine-next-state (machine-next-state (initial-machine 10 20 0) 140) 60) "coffee") "change") 50))

(begin-for-test
  (check-equal? (machine-output (machine-next-state (machine-next-state (initial-machine 0 20 0) 140) 60) "coffee") "Out of Item"))

