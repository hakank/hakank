#| 

  Person login Racket Gamble.

  From Yi Wu, Lei Li, Stuart Russell
  "BFiT: From Possible-World Semantics to Random-Evaluation Semantics in Open Universe"
  Page 3

var : numPeople
6: 0.16899999999999998
7: 0.16799999999999998
5: 0.15999999999999998
4: 0.14199999999999996
...
1: 0.005999999999999999
14: 0.004999999999999999
13: 0.002999999999999999
16: 0.0009999999999999998
mean: 6.075999999999998
Histogram:
 1: 2  
 2: 3  
 3: 26 
 4: 84 
 5: 134
 6: 184
 7: 167
 8: 153
 9: 102
10: 69 
11: 45 
12: 28 
13: 8  
14: 4  
15: 5  
16: 2  

var : sample login
1: 0.9180000000000001
0: 0.07799999999999999
2: 0.0019999999999999996
3: 0.0019999999999999996
mean: 0.9280000000000002
Histogram:
0: 65 
1: 927
2: 8  
3: 4  

var : num honest
5: 0.177
6: 0.16699999999999998
4: 0.15899999999999997
7: 0.13699999999999998
...
14: 0.0019999999999999996
15: 0.0019999999999999996
0: 0.0009999999999999998
13: 0.0009999999999999998
mean: 5.620999999999999
Histogram:
 1: 2  
 2: 7  
 3: 44 
 4: 110
 5: 181
 6: 161
 7: 175
 8: 128
 9: 98 
10: 44 
11: 35 
12: 17 
13: 7  
14: 4  
16: 2  

var : honest sample login?
#t: 0.8920000000000001
#f: 0.108
mean: 0.8920000000000001
Histogram:
#f: 112
#t: 890

  This is a port of my WebPPL model person_login.wppl.
  (which is a port of the BLOG model person_login.blog)
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")



(define (model)

  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   ; Number of people
   (define numPeople (add1 (poisson 5)))

   ; Is this person honest?
   (define (honest x) (flip 0.9))

   ;; An honest person have just one login
   (define (login x) 
        (if (honest x) 
            1
            (geometric 0.8)
            ))

   ; How many logins has these people?
   (define logins (for/list ([p numPeople])
                    (login p)))

   ; How many are honest?
   (define num_honest (sum logins))

   ; Pick one person
   (define sample_login (uniform-draw logins))

   (define honest_sample_login (honest sample_login))   
 
   (list 
        numPeople
        sample_login
        num_honest
        honest_sample_login
        
    );

   )
  )


(show-marginals (model)
                (list "numPeople"
                      "sample login"
                      "num honest"
                      "honest sample login?")
                #:num-samples 1000
                #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94
                ; #:show-stats? #t
                #:show-histogram? #t ; 10 ; #t
                ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                ; #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                )

