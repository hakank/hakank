#|
  Euler #17 in Racket

  Problem 17
  """
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
  
  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
  words, how many letters would be used?
  
  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 
  "and" when writing out numbers is in compliance with British usage.
  """


  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket

(provide (all-defined-out))

;;; (require racket/trace)
;;; (require math/number-theory)

(require (only-in "utils_hakank.rkt"
                  time-function in-range1
                  ))

;;
;;; This is a port of my Common Lisp program euler17.lisp
;;; Surprisingly few changes was needed:
;;; - (floor n 10) -> (floor (/ n 10))
;;; - added (0 "") to digits.
;;;   This was what took most time since I had to debug/trace the program for
;;;   fixing this.
;;; - removed t in (format t ...)
;;; - changed the let*/cond () parens to [], which was not really needed.
;;;

(define (digits n)
  (let ((s '((0 "") ( 1 "one") (2 "two") (3 "three") (4 "four") (5 "five")
             (6 "six") (7 "seven") (8 "eight") (9 "nine")) ))
    (second (assoc n s)))
  )

(define (teens n)
  (let ((s
         '( (10 "ten") (11 "eleven") (12 "twelve") (13 "thirteen") (14 "fourteen")
            (15 "fifteen") (16 "sixteen") (17 "seventeen") (18 "eighteen") (19 "nineteen"))))
    (second (assoc n s)))
  )
    
(define (tens n)
  (let ((s '((20 "twenty") (30 "thirty") (40 "forty") (50 "fifty") (60 "sixty")
             (70 "seventy") (80 "eighty") (90 "ninety")) ))
    (second (assoc n s))))

(define (spell n)
  (cond
    [(= n 0) ""]
    [(in-range1 n 1 9)
     ( digits n)]
    [(in-range1 n 10 19)
     (teens n)]
    [(in-range1 n 20 99)
     (let* (
            [d (* 10 (floor (/ n 10)))]
            [ten (tens d)]
            [m (modulo n 10)]
            [one (digits m)])
       (if (> m 0) 
           (format "~A~A" ten one)
           (format "~A" ten)               
           )
       )
     ]
    [(in-range1 n 100 999)
     (let* ( [hundred (floor (/ n 100))]
             [hundred1 (digits hundred)]
             [m (modulo n 100)]
             [ones (spell m)])
       (if (> m 0)
           (format "~Ahundredand~A" hundred1 ones)
           (format "~Ahundred~A" hundred1 ones)
           )
       
       )]
    [(= n 1000) "onethousand"]
    [else ""]
    )
  )

;;; cpu time: 1 real time: 1 gc time: 0
(define (euler17a)
  (for/sum ([n (range 1 1001)])
    (string-length (spell n)))
  )

(define (run)
  (time-function euler17a)
  )

(run)


