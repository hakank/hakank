#|
  Euler #19 in Racket

  Problem 19
  """
  You are given the following information, but you may prefer 
  to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not 
    on a century unless it is divisible by 400.
  
  How many Sundays fell on the first of the month during the 
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """

  This Racket program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang racket
;;; #lang at-exp racket

(provide (all-defined-out))

;;; (require math/number-theory)
;;; (require racket/trace)

(require "utils_hakank.rkt")
;;; (require infix) ; Nope, infix don't support floor


;; (define (date->julian year month day)
;;   (let* ([a (floor (/ (- 14 month) 12))]
;;          [y (- (+ year 4800) a)]
;;          [m (- (+ month (* 12 a)) 3)]
;;          [jd (+
;;               (+ day (floor (/ (+ (* 153 m) 2) 5)))
;;               (+ (* 365 y) (floor (/ y 4)))
;;               (- (floor (/ y 100)))
;;               (floor (/ y 400))
;;               (- 32045))])
;;     jd
;;     )
;;   )

;;; cpu time: 0 real time: 0 gc time: 0
(define (euler19a)
  (length (for*/list ([year (range 1901 2001)]
              [month (range 1 13)]
              #:when (= (day-of-week year month 1) 6))
    (list year month 1)
    ))
  )

(define (run)
  (time-function euler19a)
  )

(run)




