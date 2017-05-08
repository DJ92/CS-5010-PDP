;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 1: Exercise 84
;;
;; Define a function edit that consumes two inputs, Editor and KeyEvent, and
;; performs actions based on the set of KeyEvents.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require "extras.rkt")

(provide
 make-editor
 editor-pre
 editor-post
 editor?
 edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; EDITOR
(define-struct editor[pre post])

;; CONSTRUCTOR TEMPLATE : 
;;
;; An Editor is (make-editor String String)

;; INTERPRETATIONS :
;;  - pre is the string to the left of the cursor
;;  - post is the string to the right of the cursor

;; DESTRUCTOR TEMPLATE :
;;
;; editor-fn : Editor -> ??

;; (define (editor-fn ed)
;;  (...(editor-pre editor)
;;      (editor-post editor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; string-left-del : String -> String
;; GIVEN: a string of characters
;; RETURNS: string with character deleted from the end of the string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (string-left-del str)
  (substring str 0 (- (string-length str) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; string-right-del : String -> String
;; GIVEN: an input string
;; RETURNS: string with character deleted from the beginning of the string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (string-right-del str)
  (substring str 1 (string-length str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; string-insert : String String -> String
;; GIVEN : an input string and key input as string
;; RETURNS : the string after performing action based on key input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (string-insert str key)
  (string-append str key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; string-first : String -> 1String
;; GIVEN : an input string
;; RETURNS : the first 1String Character

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (string-first str)
  (string-ith str 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; string-last : String -> 1String
;; GIVEN : an input string
;; RETURNS : the last 1String Character

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

(define (string-last str)
  (string-ith str (- (string-length str) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; edit : Editor KeyEvent -> Editor
;; GIVEN: an Editor and defined set of KeyEvents
;; RETURNS: editor with string manipulated with actions based on the KeyEvents

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Editor Template

(define (edit ed key)
  (if (> (string-length (editor-pre ed)) 0)
     (cond
       [(string=? "\b" key) (make-editor (string-left-del (editor-pre ed)) (editor-post ed))]
       [(= (string-length key) 1) (make-editor (string-insert (editor-pre ed) key) (editor-post ed))]
       [(string=? "left" key)(make-editor (string-left-del (editor-pre ed)) (string-insert (string-last (editor-pre ed)) (editor-post ed)))]
       [(string=? "right" key)(make-editor (string-insert (editor-pre ed) (string-first (editor-post ed))) (string-right-del (editor-post ed)))]
     ) (make-editor (editor-pre ed) (editor-post ed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (string-left-del "dheeraj")
;  "dheera"

;; (string-right-del "joshi")
;  "oshi"

;; (string-insert "dhee" "r")
;  "dheer"

;; (string-first "DJ")
;  "D"
  
;; (string-last "dheeraj")
;  "j" 

;; (edit (make-editor "dheeraj" "joshi") "\b")
; (make-editor "dheera" "joshi")

;; (edit
;;  (edit
;;   (edit
;;    (edit
;;     (edit
;;      (edit
;;       (edit (edit (make-editor "dheeraj" "joshi") "\b") "\b") "\b") "\b") "\b") "\b") "\b") "\b") 
; "Invalid Input Key"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test
  (check-equal? (string-left-del "dheeraj") "dheera"))

(begin-for-test
  (check-equal? (string-right-del "dheeraj") "heeraj"))

(begin-for-test
  (check-equal? (string-insert "dhee" "r") "dheer"))

(begin-for-test
  (check-equal? (string-first "DJ") "D"))

(begin-for-test
  (check-equal? (string-last "dheeraj") "j"))

(begin-for-test
  (check-equal? (edit (make-editor "dheeraj" "joshi") "\b") (make-editor "dheera" "joshi")))

(begin-for-test
  (check-equal? (edit (make-editor "dheeraj" "joshi") "f") (make-editor "dheerajf"  "joshi")))

(begin-for-test
  (check-equal? (edit (make-editor "dheeraj" "joshi") "left") (make-editor "dheera"  "jjoshi")))

(begin-for-test
  (check-equal? (edit (make-editor "dheeraj" "joshi") "right") (make-editor "dheerajj"  "oshi")))

(begin-for-test
  (check-equal? 
                (edit
                 (edit
                  (edit
                   (edit (edit (make-editor "dheeraj" "joshi") "right") "right") "right") "right") "right")
                (make-editor "dheerajjoshi"  "")))

(begin-for-test
  (check-equal? 
                (edit
                 (edit
                  (edit
                   (edit
                    (edit
                   (edit (edit (make-editor "dheeraj" "joshi") "left") "left") "left") "left") "left") "left") "left")
                (make-editor ""  "dheerajjoshi")))

(begin-for-test
  (check-equal?
               (edit
                (edit
                 (edit
                  (edit
                   (edit
                    (edit
                   (edit (edit (make-editor "dheeraj" "joshi") "\b") "\b") "\b") "\b") "\b") "\b") "\b") "\b")
               (make-editor "" "joshi")))

(begin-for-test
  (check-equal?
               (edit
                (edit
                 (edit
                  (edit
                   (edit
                    (edit
                   (edit (edit (make-editor "dheeraj" "joshi") "right") "right") "right") "right") "right") "a") "b") "c")
               (make-editor "dheerajjoshiabc"  "")))

