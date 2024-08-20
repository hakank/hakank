#| 

  Sequence waiting times in Racket Gamble.

  From  Gunnar Blom, Lars Holst, Dennis Sandell:
  "Problems and Snapshots from the World of Probability"
  Page 4ff, Problem 1.4 Patterns I, (Part Problem 1)

  Problem: Waiting time for a pattern (coin flipping)
   
  What is the waiting time for the patterns(0,1) in a coin 
  flipping sequence? For the pattern (1,1)?
  Answer: 
    * waiting time for (0,1) = 4
    * waiting time for (1,1) = 6

  This model verifies the result:

Pattern: : (0 1)
var : a
(0 1): 0.2428146679881069
(0 0 1): 0.1407333994053518
(1 0 1): 0.12388503468780966
(1 0 0 1): 0.06838453914767094
(0 0 0 1): 0.0564915758176412
(1 1 0 1): 0.05550049554013872
(1 1 0 0 1): 0.03369672943508423
(0 0 0 0 1): 0.03171456888007927
(1 0 0 0 1): 0.03171456888007927
(1 1 1 0 1): 0.030723488602576794
(1 1 0 0 0 1): 0.014866204162537158
...


var : len
3: 0.26461843409316144
2: 0.2428146679881069
4: 0.18037661050545087
5: 0.12784935579781956
6: 0.06640237859266598
7: 0.05153617443012881
8: 0.026759167492566887
9: 0.02081268582755202
10: 0.007928642220019818
11: 0.0029732408325074317
12: 0.0019821605550049545
13: 0.0019821605550049545
16: 0.0009910802775024772
17: 0.0009910802775024772
14: 0.0009910802775024772
15: 0.0009910802775024772
mean: 4.023785926660056

Pattern: : (1 1)
var : a
(1 1): 0.24578790882061402
(0 1 1): 0.12091179385530207
(0 0 1 1): 0.0743310208126857
(1 0 1 1): 0.05649157581764113
(1 0 0 1 1): 0.03666997026759161
(0 0 0 1 1): 0.02675916749256685
(0 1 0 1 1): 0.024777006937561897
(0 1 0 0 1 1): 0.02180376610505447
(0 0 1 0 1 1): 0.017839444995044567
(1 0 1 0 1 1): 0.01684836471754209
(1 0 0 0 1 1): 0.013875123885034663
(0 0 0 0 1 1): 0.012884043607532187
(1 0 1 0 0 1 1): 0.011892963330029711
(0 1 0 1 0 0 1 1): 0.010901883052527235
...

var : len
2: 0.24578790882061402
4: 0.13082259663032683
3: 0.12091179385530207
5: 0.08820614469772035
6: 0.08325074331020797
8: 0.06442021803766093
7: 0.060455896927651034
9: 0.04757185332011885
10: 0.03171456888007923
12: 0.0247770069375619
13: 0.02279484638255696
11: 0.01783944499504457
14: 0.013875123885034663
16: 0.008919722497522284
15: 0.005946481665014856
17: 0.005946481665014856
19: 0.00495540138751238
18: 0.002973240832507428
22: 0.002973240832507428
23: 0.002973240832507428
20: 0.001982160555004952
25: 0.001982160555004952
26: 0.001982160555004952
27: 0.001982160555004952
33: 0.000991080277502476
35: 0.000991080277502476
39: 0.000991080277502476
28: 0.000991080277502476
31: 0.000991080277502476
mean: 5.9593657086223875

  

  This is a port of my WebPPL model sequence_waiting_time1.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

;; From hakank_utils.rkt
(define (list-slice lst [offset 0] [n (- (length lst) offset)] )
  (take (drop lst offset) n))


(define (model pattern)
  (show "Pattern: " pattern)
  (; enumerate
   rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define patternLen (length pattern))
   (define (flipSequence a)
     (let ([len (length a)])
       ; Newbie mistake: It should be equal?, not eq?
       (if (and (>= len patternLen) (equal? pattern (list-slice a (- len patternLen))))
           a
           (flipSequence (append a (list (bernoulli 0.5))))
           )
       ))
   
   (define a (flipSequence '()))
   ; (show2 "a" a "len" (length a))
  
   (list (length a)
         ; a
         )

   )
  )

(for ([pattern (list '(0 1) '(1 1))])
; (for ([pattern (list '(1 1 1) '(1 0 1) '(1 1 0) '(0 1 1))])  
  (show-marginals (model pattern)
                (list "len"
                      "a"
                      )
                  #:num-samples 10000
                  ; #:truncate-output 20
                  ; #:show-stats? #t
                  ; #:credible-interval 0.84
                  ; #:credible-interval2 0.93                  
                  ; #:skip-marginals? #t
                  #:show-histogram? #t
                  )
)
