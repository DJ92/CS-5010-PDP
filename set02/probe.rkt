;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Question 4:
;;
;; Your space probe to Pluto has just landed. Here's the situation:

;; 1. The probe is a circle, 40cm in diameter.

;; 2. At every step, the probe can move forward some number of steps, or it can rotate 90 degrees
;; either right or left. The probe moves by taking steps of exactly 1 cm.

;; 3. The Plutonians, anticipating the arrival of our probe, have constructed a trap in the form
;; of a square with side 347 cm, centered on the origin.

;; 4. The probe has landed right in the trap! It has landed with its center at the origin, facing north (up).

;; 5. We will use graphics-style coordinates instead of standard mathematical coordinates.
;; That is, when the probe moves north, its y-coordinate DECREASES. So the northernmost wall of the trap
;; is at y = -173.5 .

;; 6. The probe can also sense when it has run into a wall. If any move of the probe would cause
;; the probe to run into the wall of the trap, then the probe will move forward until
;; another 1cm step would take it past the wall, and then stop. For example, if the probe proceeded north
;; from its initial position, it could move only 153 cm, since that puts its northernmost edge at (0, -173),
;; and another 1cm step would send it crashing into the wall.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Required for Homework Submission

(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")
(provide
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS :

;; PROBE : 

(define-struct probe(xpos ypos dir))

;; CONSTRUCTOR TEMPLATE : 
;;
;; A Probe is (make-probe Integer Integer String)

;; INTERPRETATIONS :
;;  - xpos is the x-coordinate
;;  - ypos is the y-coordinate
;;  - dir is the direction

;; DESTRUCTOR TEMPLATE :
;;
;; probe-fn : Probe -> ??

;; (define (probe-fn pr)
;;  (...(probe-xpos probe)
;;      (probe-ypos probe)
;;      (probe-dir probe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; probe-at : Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; RETURNS: a probe with its center at those coordinates, facing north (default direction).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Template Probe

(define (probe-at x y)(make-probe x y "north"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; probe-turned-left : Probe -> Probe
;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees either left
;;          or right.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Function Composition
(define (probe-turned-left pr)
  (cond[(string=? (probe-dir pr) "north") (make-probe (probe-xpos pr) (probe-ypos pr) "west")]
       [(string=? (probe-dir pr) "west") (make-probe (probe-xpos pr) (probe-ypos pr) "south")]
       [(string=? (probe-dir pr) "south") (make-probe (probe-xpos pr) (probe-ypos pr) "east")]
       [(string=? (probe-dir pr) "east") (make-probe (probe-xpos pr) (probe-ypos pr) "north")]))

(define (probe-turned-right pr)
  (cond[(string=? (probe-dir pr) "north") (make-probe (probe-xpos pr) (probe-ypos pr) "east")]
       [(string=? (probe-dir pr) "east") (make-probe (probe-xpos pr) (probe-ypos pr) "south")]
       [(string=? (probe-dir pr) "south") (make-probe (probe-xpos pr) (probe-ypos pr) "west")]
       [(string=? (probe-dir pr) "west") (make-probe (probe-xpos pr) (probe-ypos pr) "north")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; probe-forward : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward by the
;;          specified distance.  If moving forward the specified distance would
;;          cause the probe to hit any wall of the trap, then the probe should 
;;          move as far as it can inside the trap, and then stop.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; Use Template Probe
;; - Simplified strategy (Follow destructor template)

(define (probe-forward pr dist)
  (cond [(and (probe-north? pr) (< (+ (probe-ypos pr) dist) 153)) (make-probe (probe-xpos pr) (+ (probe-ypos pr) dist) "north")]
        [(and (probe-north? pr) (> (+ (probe-ypos pr) dist) 153)) (make-probe (probe-xpos pr) 153 "north")]
        [(and (probe-south? pr) (> (- (probe-ypos pr) dist) -153)) (make-probe (probe-xpos pr) (- (probe-ypos pr) dist) "south")]
        [(and (probe-south? pr) (< (- (probe-ypos pr) dist) -153)) (make-probe (probe-xpos pr) -153 "south")]
        [(and (probe-west? pr) (> (- (probe-xpos pr) dist) -153)) (make-probe (- (probe-xpos pr) dist) (probe-ypos pr) "west")]
        [(and (probe-west? pr) (< (- (probe-xpos pr) dist) -153)) (make-probe -153 (probe-ypos pr) "west")]
        [(and (probe-east? pr) (< (+ (probe-xpos pr) dist) 153)) (make-probe (+ (probe-xpos pr) dist) (probe-ypos pr) "east")]
        [(and (probe-east? pr) (> (+ (probe-xpos pr) dist) 153)) (make-probe 153 (probe-ypos pr) "east")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONTRACT & PURPOSE STATEMENT :

;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> Boolean
;; GIVEN: a probe
;; ANSWERS: whether the probe is facing in the specified direction.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DESIGN STRATEGY :

;; - no cond statements

(define (probe-north? pr)
  (cond[(string=? (probe-dir pr) "north") true]
       [else false]))

(define (probe-south? pr)
  (cond[(string=? (probe-dir pr) "south") true]
       [else false]))

(define (probe-east? pr)
  (cond[(string=? (probe-dir pr) "east") true]
       [else false]))

(define (probe-west? pr)
  (cond[(string=? (probe-dir pr) "west") true]
       [else false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES :

;; (probe-at 50 50)
;  (make-probe 50 50 "north")

;; (probe-north? (make-probe 50 50 "north"))
;  #true

;; (probe-east? (make-probe 50 50 "west"))
;  #false

;; (probe-south? (make-probe 50 50 "south"))
;  #true

;; (probe-west? (make-probe 50 50 "north"))
;  #false

;; (probe-forward (make-probe -10 10 "west") 144)
;  (make-probe -153 10 "west")

;; (probe-forward (make-probe 10 10 "east") 144)
;  (make-probe 153 10 "east")

;; (probe-forward (make-probe 10 10 "south") 164)
;  (make-probe 10 -153 "south")

;; (probe-forward (make-probe 10 10 "north") 154)
;  (make-probe 10 153 "north")

;; (probe-turned-left (make-probe 10 10 "north"))
;  (make-probe 10 10 "west")

;; (probe-turned-right (make-probe 10 10 "north"))
;  (make-probe 10 10 "east")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES :

(begin-for-test
  (check-equal? (probe-at 50 50) (make-probe 50 50 "north")))

(begin-for-test
  (check-equal? (probe-north? (make-probe 50 50 "north")) true))

(begin-for-test
  (check-equal? (probe-east? (make-probe 50 50 "west")) false))

(begin-for-test
  (check-equal? (probe-south? (make-probe 50 50 "south")) true))

(begin-for-test
  (check-equal? (probe-west? (make-probe 50 50 "north")) false))

(begin-for-test
  (check-equal? (probe-forward (make-probe -10 10 "west") 144) (make-probe -153 10 "west")))

(begin-for-test
  (check-equal? (probe-forward (make-probe 10 10 "east") 144) (make-probe 153 10 "east")))

(begin-for-test
  (check-equal? (probe-forward (make-probe 10 10 "south") 164) (make-probe 10 -153 "south")))

(begin-for-test
  (check-equal? (probe-forward (make-probe 10 10 "north") 154) (make-probe 10 153 "north")))

(begin-for-test
  (check-equal? (probe-turned-left (make-probe 10 10 "north")) (make-probe 10 10 "west")))

(begin-for-test
  (check-equal? (probe-turned-right (make-probe 10 10 "north")) (make-probe 10 10 "east")))
