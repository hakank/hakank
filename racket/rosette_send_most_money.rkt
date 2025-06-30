#| 

  SEND+MOST=MONEY in Racket/Rosette.

  Search a solution with distinct values for each variables
  that satisfies:
    SEND
    MOST
  + ====
   MONEY 
  
  and optimise the value of MONEY.
  and find all optimal values of MONEY.


  (Cf the simpler rosette_send_more_money.rkt)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang rosette

(provide (all-defined-out))
;;; (require racket/trace)
(require "rosette_utils.rkt")

(require rosette/solver/smt/z3)


#|

(send-most-money optimize #f)
(h #hash((money . 10438) (o . 0) (n . 4) (opt . 10438) (t . 6) (d . 2) (s . 9) (y . 8) (m . 1) (e . 3)))
(h #hash((money . 10548) (o . 0) (n . 5) (opt . 10548) (t . 6) (d . 2) (s . 9) (y . 8) (m . 1) (e . 4)))
(h #hash((money . 10437) (o . 0) (n . 4) (opt . 10437) (t . 5) (d . 2) (s . 9) (y . 7) (m . 1) (e . 3)))
(h #hash((money . 10657) (o . 0) (n . 6) (opt . 10657) (t . 3) (d . 4) (s . 9) (y . 7) (m . 1) (e . 5)))
(h #hash((money . 10875) (o . 0) (n . 8) (opt . 10875) (t . 3) (d . 2) (s . 9) (y . 5) (m . 1) (e . 7)))
(h #hash((money . 10876) (o . 0) (n . 8) (opt . 10876) (t . 2) (d . 4) (s . 9) (y . 6) (m . 1) (e . 7)))
(h #hash((money . 10875) (o . 0) (n . 8) (opt . 10875) (t . 2) (d . 3) (s . 9) (y . 5) (m . 1) (e . 7)))
(h #hash((money . 10876) (o . 0) (n . 8) (opt . 10876) (t . 4) (d . 2) (s . 9) (y . 6) (m . 1) (e . 7)))
(h #hash((money . 10548) (o . 0) (n . 5) (opt . 10548) (t . 2) (d . 6) (s . 9) (y . 8) (m . 1) (e . 4)))
(h #hash((money . 10657) (o . 0) (n . 6) (opt . 10657) (t . 4) (d . 3) (s . 9) (y . 7) (m . 1) (e . 5)))
(h #hash((money . 10438) (o . 0) (n . 4) (opt . 10438) (t . 2) (d . 6) (s . 9) (y . 8) (m . 1) (e . 3)))
(h #hash((money . 10437) (o . 0) (n . 4) (opt . 10437) (t . 2) (d . 5) (s . 9) (y . 7) (m . 1) (e . 3)))
(h #hash((money . 10768) (o . 0) (n . 7) (opt . 10768) (t . 3) (d . 5) (s . 9) (y . 8) (m . 1) (e . 6)))
(h #hash((money . 10768) (o . 0) (n . 7) (opt . 10768) (t . 5) (d . 3) (s . 9) (y . 8) (m . 1) (e . 6)))
(h #hash((money . 10765) (o . 0) (n . 7) (opt . 10765) (t . 2) (d . 3) (s . 9) (y . 5) (m . 1) (e . 6)))
(h #hash((money . 10765) (o . 0) (n . 7) (opt . 10765) (t . 3) (d . 2) (s . 9) (y . 5) (m . 1) (e . 6)))
(number of solutions 16)
cpu time: 93 real time: 601 gc time: 18

(send-most-money optimize #t)
(get-max-solution (s e n d m o t y money) opt)
Optimal solutions:
((9 7 8 4 1 0 2 6 10876))
(10876)
((9 7 8 2 1 0 4 6 10876))
cpu time: 28 real time: 262 gc time: 0
cpu time: 121 real time: 864 gc time: 18

  
|#
(define (send-most-money #:optimize? [optimize? #f]) 
  (show "send-most-money" "optimize" optimize?)
  (clear-vc!)

  (current-solver (z3 #:logic 'QF_FD))
  ; (current-solver (z3))
  
  (define-symbolic s e n d m o t y money opt integer?)
  
  (define vars (list s e n d m o t y money))
  
  ; domains
  ;; (assume (<= 0 s 9))
  ;; (assume (<= 0 e 9))
  ;; (assume (<= 0 n 9))
  ;; (assume (<= 0 d 9))
  ;; (assume (<= 0 m 9))
  ;; (assume (<= 0 o 9))
  ;; (assume (<= 0 t 9))
  ;; (assume (<= 0 y 9))
  (for ([v (list s e n d m o t y)])
    (assume (<= 0 v 9)))

  (assume (<= 0 money 999999))
  (assume (<= 0 opt 999999))  
  
  (assume (> s 0))
  (assume (> m 0))        

  ; (assume (distinct? s e n d m o t y))
  ; (assume (apply distinct? vars))
  (assume (all-different vars))
    
  ; To optimize
  (assume (= money (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y)))
  
  (assume (=
           (+
            (+            (* 1000 s) (* 100 e) (* 10 n) d)
            (+            (* 1000 m) (* 100 o) (* 10 s) t))
           money
           ))

  ; The objective must be called opt! (TODO: Fix this!)
  (assume (= money opt))
  
  (if optimize?
      (let ([max-solution (get-max-solution vars opt #:get-all-opt? #t #:debug? #f)])
        (displayln "Optimal solutions:")
        (for ([sol max-solution])
          (show sol))
        )
      (let ([all-solutions (get-all-solutions vars #:debug? #t)])
          (show "number of solutions" (length all-solutions))
          )
      )
  )

(time (begin
        (time (send-most-money #:optimize? #f))
        (newline)
        (clear-vc!)
        (time (send-most-money #:optimize? #t ))
        (clear-vc!)
        ))

#|

  Using solver-maximize and solver-check
  Note: This only yield one optimal solution.
  This is faster

  TODO: Perhaps I can add constraints to find all optimal solutions?


(model
 [s 9]
 [e 7]
 [n 8]
 [d 4]
 [m 1]
 [o 0]
 [t 2]
 [y 6]
 [money 10876])

|#
(define (send-most-money2)
  (newline)
  (show "send-most-money2")
  (clear-vc!)
  
  ; (define solver (z3 #:logic 'QF_FD))
  (define solver (z3))
  
  (define-symbolic s e n d m o t y money opt integer?)  
  (define vars (list s e n d m o t y money))
  (define constraints (flatten (list ;; (<= 0 s 9)
                                ;; (<= 0 e 9)
                                ;; (<= 0 n 9)
                                ;; (<= 0 d 9)
                                ;; (<= 0 m 9)
                                ;; (<= 0 o 9)
                                ;; (<= 0 t 9)
                                ;; (<= 0 y 9)
                                (for/list ([v (list s e n d m o t y)])
                                  (<= 0 v 9))
                                (> s 0)
                                (> m 0)        
                                
                                ; (assume (distinct? s e n d m o t y))
                                ; (apply distinct? vars)
                                (all-different vars)
                                
                                ; To optimize
                                (= money (+ (* 10000 m) (* 1000 o) (* 100 n) (* 10 e) y))
                                
                                (=
                                 (+
                                  (+            (* 1000 s) (* 100 e) (* 10 n) d)
                                  (+            (* 1000 m) (* 100 o) (* 10 s) t))
                                 money
                                 ))))
    
    (solver-assert solver constraints)
    (solver-maximize solver (list money))
    (solver-check solver)
    
    )

(time (send-most-money2))

