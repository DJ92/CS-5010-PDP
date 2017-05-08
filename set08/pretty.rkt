;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem Set: 08

;; Question 1:

;; Deliever Pretty.rkt which contains pretty printers for expressions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide
 expr-to-strings
 make-sum-exp
 make-diff-exp
 sum-exp-exprs
 diff-exp-exprs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                            ;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPRESSION

;; An Expr is an expression which is one of
;; -- Integer
;; INTERPRETATION: an Integer
;; -- (make-sum-exp NELOExpr)
;; INTERPRETATION: (make-sum-exp NELOExpr) is a sum-exp represents a sum
;; -- (make-diff-exp NELOExpr)
;; INTERPRETATION: (make-diff-exp NELOExpr) a diff-exp
;; represents a difference calculation.

;; TEMPLATE
;; (define (expr-fn e)
;;  (cond
;;    [(integer? e) ...]
;;    [(sum-exp? e)...]
;;    [else (diff-fn? e)...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LIST OF EXPRESSION

;; A LOExpr is one of
;; -- empty
;; INTERPRETATION: the list is empty and has no expressions 
;; -- (cons Expr LOExpr)
;; INTERPRETATION: (cons Expr LOExpr) is a sequence of expressions such that
;;                 the first element Expr is a expression and rest of the list
;;                 is denoted by LOExpr.

;; TEMPLATE:
;; loe-fn : LOExpr -> ??
;; (define (loe-fn loe)
;;  (cond
;;    [(empty? loe) empty]
;;    [else (...
;;           (expr-fn (first loe))
;;           (loe-fn (rest loe)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NON-EMPTY LIST OF EXPRESSION

;; A NELOExpr is a non-empty LOExpr.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SUM EXPRESSION

(define-struct sum-exp (exprs))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Sum exp is a (make-sum-exp exprs)

;; INTERPRETATION: (make-sum-exp exprs) is a sum expression where
;; expr is an NELOExpr.

;; DESTRUCTOR TEMPLATE :
;;
;; sum-exp-fn : NELOExpr -> ??

;; (define (sum-exp-fn e)
;;  (loe-fn(sum-exp-exprs e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DIFFERENCE EXPRESSION

(define-struct diff-exp (exprs))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Diff exp is a (make-diff-exp exprs)

;; INTERPRETATION: (make-diff-exp exprs) is a difference expression where
;; exprs is an NELOExpr.

;; DESTRUCTOR TEMPLATE :
;;
;; diff-exp-fn : NELOExpr -> ??

;; (define (diff-exp-fn e)
;;  (loe-fn(diff-exp-exprs e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LIST OF STRING

;; A ListOfString is one of
;; -- empty
;; INTERPRETATION: the list is empty and has no strings
;; -- (cons s los)
;; INTERPRETATION: (cons s los) is a sequence of strings such that
;;                 the first element s is a string and rest of the list
;;                 is denoted by los.

;; TEMPLATE:
;; los-fn : ListOfString -> ??
;; (define (los-fn los)
;;  (cond
;;    [(empty? los) empty]
;;    [else (...
;;           (... (first los))
;;           (los-fn (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LIST OF X :

;; A ListOfX is one of -

;; - empty
;; Interpretation : list of x is empty. 

;; - (cons x lox)
;; Interpretation : (cons x lox) represents a sequence of x's
;;                  whose first element x is a ListOfString
;;                  and other x's are represented by lox.

;; lox-fn : ListOfX -> ??
;; (define (lox-fn lox)
;;   (cond
;;     [(empty? lox) ...]
;;     [else (...
;;            (los-fn(first lox))
;;            (lox-fn (rest lox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                ;; CONSTANTS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INITIAL CURSOR INDEX
(define CURSOR 0)

;; COUNT FOR WIDTH CHECK
(define COUNT 0)

;; DEFAULT INTEGER SPACE-COUNT
(define INT-SPACE 1)

;; DEFAULT EXP SPACE-COUNT
(define EXP-SPACE 4)

;; SPACING COUNT
(define SPACE-COUNT 0)

;; SPACING FACTOR
(define FACTOR 3)

;; SPACE STRING
(define SPACE " ")

;; SPACE CHAR
(define SPACE-CHAR #\space)

;; SUM OPERATOR
(define SUM-OP "(+ ")
(define SUM-OP-ALT "(+")

;; DIFFERENCE OPERATOR
(define DIFF-OP "(- ")
(define DIFF-OP-ALT "(-")

;; ENDING BRACKET
(define END-EXP ")")

;; DEFAULT STARTING STRING
(define START "")

;; DEFAULT ENDING STRING
(define END "")

;; EMPTY LOS
(define LIST-OF-STRINGS empty)

;; ERROR
(define ERROR-MSG "not enough room")

;; EXAMPLES
(define HW-EXAMPLE-1 (make-sum-exp (list 22 333 44)))
(define HW-EXAMPLE-2 (make-sum-exp
                      (list
                       (make-diff-exp (list 22 3333 44))
                       (make-diff-exp
                        (list
                         (make-sum-exp (list 66 67 68))
                         (make-diff-exp (list 42 43))))
                       (make-diff-exp (list 77 88)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-to-strings : Expr NonNegInt -> ListOfString
;; GIVEN: An expression expr and width w
;; RETURNS: A representation of the expression as a sequence of lines, with
;; each line represented as a string of length not greater than the width.

;; STRATEGY: Combine Simpler Functions

(define (expr-to-strings expr w)
    (convert-expr-to-los expr w LIST-OF-STRINGS START END SPACE-COUNT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert-expr-to-los : Expr NonNegInt ListOfString String String NonNegInt
;;                                                   -> ListOfString
;; GIVEN: An expression as expr, width as w, a ListOfString as los,
;; a starting string as start, an ending string as end and the level as n.
;; WHERE: los is the list of string representation of all expressions, start
;; is used to add '(+ ' and '(- ' from main expression to the start of sub
;; expressions. end is for main and sub expressions and adds the closing
;; parenthesis. n keeps track of the number of spaces to add before the sub
;; expressions with respect to all the parent expressions.
;; RETURNS: A representation of the expression as a sequence of lines, with
;; each line represented as a string of length not greater than the width.
;; HALTING MEASURE: when the difference between the width and the
;; sum of levels and width of the last level subexpression is negative.
;; TERMINATION ARGUMENT: After the recursion is complete and no expressions
;; are left to be evaluated and if the width of the expression or the
;; subexpression is more than the allowed width, the difference
;; becomes negative.

;; STRATEGY: Cases on Expression expr

(define (convert-expr-to-los expr w los start end n)
  (if (expr-out-of-width? expr w n)
      (append (list (convert-exp-to-string expr start end CURSOR n)) los)
      (check-before-exp-to-los expr w start end los n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-out-of-width?: Expr NonNegInt -> Boolean
;; GIVEN: An Expression Expr and NonNegInt w
;; WHERE: w is the total width allowed for all expressions
;; RETURNS: true iff the width of the expression is greater than width w

;; STRATEGY: Combine Simpler Functions

(define (expr-out-of-width? expr w n)
  (> w (+ (expr-width expr) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-before-exp-to-los : Expr NonNegInt ListOfString String String NonNegInt
;;                                                               -> ListOfString
;; GIVEN: An Expression exp, a NonNegInt width w, a NonNegInt level n,
;; a String start, A String end, A ListOfString los.
;; RETURNS: a ListOfStrings containing all the expressions.
;; HALTING MEASURE: The halting measure is already defined in the function above
;; Therefore, if the expression cannot be further broken down into a
;; subexpression, we throw error using the error function.
;; TERMINATION ARGUMENT: A subexpression cannot be split if it is an Integer.
;; Therefore, it won't fit within the given width and hence, it wont be able to
;; accomodate the expression in the given width.

;; STRATEGY: Use template for Expr on exp

(define (check-before-exp-to-los exp w start end los n)
  (cond[(sum-exp? exp)
        (convert-subexp-to-los (sum-exp-exprs exp)
                               w
                               (string-append start SUM-OP)
                               (string-append end END-EXP)
                               los
                               (+ FACTOR n))]
       [(diff-exp? exp)
        (convert-subexp-to-los (diff-exp-exprs exp)
                               w
                               (string-append start DIFF-OP)
                               (string-append end END-EXP)
                               los
                               (+ FACTOR n))]
       [else (error ERROR-MSG)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert-subexp-to-los : NELOExpr NonNegInt NonNegInt String String
;;                                                  ListOfString -> ListOfString
;; GIVEN: A NELOExpr loe, a NonNegInt width w, a String start, A String end,
;; A ListOfString los and NonNegInt level n.
;; WHERE: Same as above. The los will contain the string representation of all
;; the expressions until the current recursion.
;; RETURNS: a ListOfStrings containing the formatted expressions.
;; HALTING MEASURE: Same as above.
;; TERMINATION ARGUMENT: Same as above.

;; STRATEGY: Use HOF map on NELOExpr loe

(define (convert-subexp-to-los loe w start end los n)
  (combine-all-items (map
                      (lambda(e)(build-loe e loe w start end los n))
                      loe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-loe : Expr NELOExpr NonNegInt ListOfString  String String NonNegInt
;;                                                               -> ListOfString
;; GIVEN: An Expression e, A NELOExpr loe, a NonNegInt max-width, a NonNegInt
;; n, a String start, A String end, A ListOfString los.
;; WHERE: Same as above.
;; RETURNS: a ListOfStrings containing the formatted expressions.
;; NOTE: If the given expr e is the first subexpresssion of the main
;;  expressions NELOExpr loe, we need to add pass the start along with the 
;;  expr. If it the last expression, we need to pass the closing paranthesis.
;;  Else, we pass a blank string start and end.

;; STRATEGY: Use general recursion on NELOExpr loe

(define (build-loe exp loe w start end los n)
  (cond[(equal? exp (first loe))
        (append (convert-expr-to-los exp w los start END n) los)]
       [(equal? exp (last loe))
        (append (convert-expr-to-los exp w los START end n) los)]
       [else (append (convert-expr-to-los exp w los START END n) los)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert-exp-to-string: Expr NonNegInt String String NonNegInt -> String
;; GIVEN: An Expression exp, Strings start and end, A NonNegInt cursor
;;         A NonNegInt n
;; RETURNS: The formatted String with all the given attributes added to it. 
;; WHERE: n, start, end are passed down from the function above.
;; cursor is a NonNegInt used to indicate the position of the cursor for the
;; current expression, so that the expressions can be added to the end of one
;; other beautifully.
;; HALTING MEASURE: if current expression is an Integer
;; TERMINATION ARGUMENT: When the current expression is an Integer, there is no
;; subexpression, so we append current expression to a string upto
;; the current iteration and the program will automatically stop.

;; STRATEGY: Use template for Expr on exp

(define (convert-exp-to-string exp start end c n)
  (cond[(integer? exp)
        (build-exp-string exp start end c LIST-OF-STRINGS START n)]
       [(sum-exp? exp)
        (build-exp-string exp start end c (sum-exp-exprs exp) SUM-OP-ALT n)]
       [(diff-exp? exp)
        (build-exp-string exp start end c (diff-exp-exprs exp) DIFF-OP-ALT n)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-exp-string: Expr NonNegInt String String NonNegInt LOExpr String
;                                                                   -> String
;; GIVEN: An Expression exp, A NonNegInt n, Strings start and end,
;; A NonNegInt cursor c, An NELOExpr loe, A string temp-start ts.
;; RETURNS: The formatted String with all the given attributes added to it. 
;; WHERE: Same as above. temp-start is a String that is used to pass the local
;; start "(+" or "(-" from the calling function. loe can be the NELOExpr of the
;; parent expression or empty in case of a number.

;; STRATEGY: Combine Simpler Functions

(define (build-exp-string exp start end c lst ts n)
  (string-append (build-exp-start start n)
                 (get-string-using-cursor c)
                 ts
                 (build-subexp-string lst exp c)
                 end
                 (if (string=? ts START)
                     END
                     END-EXP)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-string-using-cursor: NonNegInt -> String
;; GIVEN: A NonNegInt c
;; RETURNS: The String to be appended
;; WHERE: the cursor c is a flag to mark the position of the intial character.

;; STRATEGY: Cases on c

(define (get-string-using-cursor c)
  (if (> c CURSOR)
      SPACE
      END))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-subexp-string: NELOExpr Expr NonNegInt -> String
;; GIVEN: An NELOExpr loe, An Expression exp, A NonNegInt cursor. 
;; RETURNS: The formatted String with all the given attributes added to it. 
;; WHERE: Same as above. 

;; STRATEGY: Combine Simpler Functions

(define (build-subexp-string loe exp c)
  (if (empty? loe)
      (convert-integer-to-string exp)
      (build-next-subexp-string loe (+ FACTOR c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-exp-start : String NonNegInt -> String
;; GIVEN: A String start and a NonNegInt n.
;; RETURNS: A String the start or the given number of white spaces.
;; WHERE: Same as above.

;; STRATEGY: Combine Simpler Functions

(define (build-exp-start start n)
  (if (string=? start START)
      (make-string n SPACE-CHAR)
      (build-next-exp-start start n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-next-exp-start : String NonNegInt -> String
;; GIVEN: A String start and a NonNegInt n
;; RETURNS: A String containing the start and optional spaces.

;; STRATEGY: Combine Simpler Functions

(define (build-next-exp-start start n)
  (if (> n (string-length start))
      (string-append (make-string (- n (string-length start)) SPACE-CHAR)
                     start)
      start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-next-subexp-string : NELOExpr NonNegInt -> String
;; GIVEN: An NELOExpr loe, A NonNegInt c. 
;; RETURNS: A String with all the sub expressions joined together. 

;; STRATEGY: Use HOF foldl on loe

(define (build-next-subexp-string loe c)
  (foldl
   (lambda(exp e)(string-append e (convert-exp-to-string
                                   exp
                                   START
                                   END
                                   c
                                   SPACE-COUNT)))
   END
   loe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expr-width : Expr -> NonNegInt
;; GIVEN: An Expression exp. 
;; RETURNS: the total number of characters including operator strings,
;;          paranthesis and spaces in the expression.

;; STRATEGY: Cases on exp

(define (expr-width exp)
  (if (integer? exp)
      (string-length (convert-integer-to-string exp))
      (- (get-expr-width exp COUNT) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-expr-width : Expr NonNegInt -> NonNegInt
;; GIVEN: An Expression exp and a NonNegInt cnt. 
;; RETURNS: the count of characters in the expression.
;; WHERE: cnt is number of characters upto the current expression.
;; HALTING MEASURE: when exp is a number.
;; TERMINATION ARGUMENT: Since, a number cannot have further nested expressions,
;;     we stop recursion and return the value.

;; STRATEGY: Use template for Expr on exp

(define (get-expr-width exp cnt)
  (cond[(integer? exp) (+ cnt
                          INT-SPACE
                          (string-length (convert-integer-to-string exp)))]
       [(sum-exp? exp) (+ cnt
                          EXP-SPACE
                          (cal-width-of-subexp (sum-exp-exprs exp)))]
       [(diff-exp? exp) (+ cnt
                           EXP-SPACE
                           (cal-width-of-subexp (diff-exp-exprs exp)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cal-width-of-subexp : NELOExpr -> NonNegInt
;; GIVEN: A NELOExpr loe
;; RETURNS: the total number of characters in all the expressions.

;; STRATEGY: Use HOF foldr on NELOExpr loe

(define (cal-width-of-subexp loe)
  (foldr
   (lambda(exp e)(+ e (get-expr-width exp COUNT)))
   COUNT
   loe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert-integer-to-string : Integer -> String
;; GIVEN: an Integer
;; RETURNS: a string into which the integer is type casted.

;; STRATEGY : Combine Simpler Functions

(define (convert-integer-to-string expr)
  (number->string expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; combine-all-items : ListOfString -> ListOfString
;; GIVEN: a ListOfString lst
;; RETURNS: a list with all the items in the list combined into the first
;; element

;; STRATEGY : Use HOF foldr on ListOfString lst

(define (combine-all-items lst)
  (foldr
   (lambda(n e)(append n e))
   empty
   lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; last : NELOExpr -> Expr
;; GIVEN: a ListOfExpr loe
;; RETURNS: the last element of the list

;; STRATEGY : Combine Simpler Functions

(define (last loe)
  (cond [(empty? loe) empty]
        [else (first (reverse loe))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES:

(begin-for-test
  (check-equal? (expr-to-strings HW-EXAMPLE-1 15)
                (list "(+ 22 333 44)"))
  (check-equal? (expr-to-strings HW-EXAMPLE-1 10)
                (list "(+ 22" "   333" "   44)"))
  (check-equal? (expr-to-strings HW-EXAMPLE-1 25)
                (list "(+ 22 333 44)"))
  (check-equal? (expr-to-strings HW-EXAMPLE-2 100)
                (list
                 "(+ (- 22 3333 44) (- (+ 66 67 68) (- 42 43)) (- 77 88))"))
  (check-equal? (expr-to-strings HW-EXAMPLE-2 50)
               (list
                "(+ (- 22 3333 44)"
                "   (- (+ 66 67 68) (- 42 43))"
                "   (- 77 88))"))
  (check-equal? (expr-to-strings HW-EXAMPLE-2 20)
                (list
                 "(+ (- 22 3333 44)"
                 "   (- (+ 66 67 68)"
                 "      (- 42 43))"
                 "   (- 77 88))"))
  (check-equal? (expr-to-strings HW-EXAMPLE-2 15)
                (list
                 "(+ (- 22"
                 "      3333"
                 "      44)"
                 "   (- (+ 66"
                 "         67"
                 "         68)"
                 "      (- 42"
                 "         43))"
                 "   (- 77 88))"))
  (check-equal? (last empty) empty))