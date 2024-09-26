#| 

  Birthday in Racket Gamble.

  From BLOG Swift example birthday.blog
  https://github.com/lileicc/swift/blob/master/example/birthday.blog
  """
  Distribution of values for 
   exists Person x (exists Person y ((!(x = y) & (Birthday(x) = Birthday(y)))))
	true	0.5446927374301685
	false	0.4553072625698328
  """

  Closed world = #t means that we observe that there are 23 people in the room.

 Closed world: : #f
var : N
12: 0.10999999999999997
13: 0.10599999999999998
14: 0.09999999999999998
15: 0.09799999999999999
11: 0.09399999999999997
10: 0.08299999999999999
16: 0.07999999999999999
9: 0.061999999999999986
17: 0.061999999999999986
8: 0.04499999999999999
18: 0.03799999999999999
7: 0.029999999999999992
19: 0.023999999999999994
20: 0.023999999999999994
6: 0.017999999999999995
22: 0.007999999999999998
5: 0.006999999999999998
21: 0.005999999999999998
4: 0.0019999999999999996
3: 0.0009999999999999998
25: 0.0009999999999999998
26: 0.0009999999999999998
mean: 13.020999999999995

var : b
0: 0.7719999999999999
2: 0.20499999999999996
4: 0.018
6: 0.003999999999999999
10: 0.0009999999999999998
mean: 0.5159999999999999

var : p
#f: 0.7719999999999999
#t: 0.22799999999999998
mean: 0.22799999999999998

 Closed world: : #t
var : N
23: 1.0
mean: 23.0

var : b
0: 0.496
2: 0.371
4: 0.101
6: 0.029
8: 0.002
10: 0.001
mean: 1.3459999999999999

var : p
#t: 0.504
#f: 0.496
mean: 0.504



  This is a port of my WebPPL model birthday.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model closed_world)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define day (range 365))

   (defmem (birthday b) (uniform-draw day))
    
   (defmem (numPeople) (poisson 13))
       
   ;; "comment the following to try full open world model"
   ; (when closed_world (observe/fail (= (sample numPeople) 23) ))
   (when closed_world (observe/fail (= (numPeople) 23) ))
   ; (when closed_world (observe-sample (numPeople) 23))

   ;; is there a duplicate of birthdays?
   (define b (for/sum ([x (numPeople)])
                       (for/sum ([y (numPeople)])
                         (if (and (not (= x y)) (= (birthday x) (birthday y))) 1 0))))
   (define p (> b 0))
   
   
   (list (numPeople)
         b
         p
       )

   )
)


(for ((closed_world '(#f #t)))
      (show " Closed world: " closed_world)
      (show-marginals (model closed_world)
                      (list  "N"
                             "b"
                             "p"
                             )
                      #:num-samples 1000
                      ; #:truncate-output 5
                      ; #:skip-marginals? #t
                      ; #:show-stats? #t
                      ; #:credible-interval 0.84
                      ; #:show-histogram? #t
                      ; #:show-percentiles? #t
                      )
      )

