#| 

  Successive wins in Racket/Gamble 

  From Mosteller "Fifty Challenging problems in Probability"
  """
  Problem 2: Successive Wins
        
  To encourage Elmer's promising tennis career, his 
  father offers him a prize if he wins (at least) two 
  tennis sets in a row in a three-set series to be played 
  with his father and the club champion alternately:  
  father-champion-father or champion-father-champion, 
  according to Elmer's choice.  The champion is a better 
  player than Elmer's father.  Which series should Elmer 
  choose?
  """

  It depends on Elmer's probability of winning against
  the father and the champion.

  Probability of Elmer winning against the father is 0,2 
  and against the champion 0.4:

  var : matches1
  (0 0 0): 36/125 (0.288)
  (0 0 1): 24/125 (0.192)
  (1 0 0): 24/125 (0.192)
  (1 0 1): 16/125 (0.128)
  (0 1 0): 9/125 (0.072)
  (1 1 0): 6/125 (0.048)
  (0 1 1): 6/125 (0.048)
  (1 1 1): 4/125 (0.032)

  var : matches2
  (0 0 0): 48/125 (0.384)
  (0 1 0): 32/125 (0.256)
  (0 0 1): 12/125 (0.096)
  (1 0 0): 12/125 (0.096)
  (1 1 0): 8/125 (0.064)
  (0 1 1): 8/125 (0.064)
  (1 0 1): 3/125 (0.024)
  (1 1 1): 2/125 (0.016)

  var : success1
  #f: 109/125 (0.872)
  #t: 16/125 (0.128)
  mean: 16/125 (0.128)

  var : success2
  #f: 107/125 (0.856)
  #t: 18/125 (0.144)
  mean: 18/125 (0.144)

  I.e. the probability of winning two successive matches for the two sequences:
  * (father champion father)  : 12.8%
  * (champion father champion): 14.4%


  Here's a more extensive test of some different probabilities. Note that
  the champion is better than the father.

  Prob winning against father: 0 champion: 1/1000: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 1/5: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 2/5: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 1/2: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 3/5: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 4/5: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 99/100: seq1:0 seq2:0 best:same
  Prob winning against father: 0 champion: 1: seq1:0 seq2:0 best:same

  Prob winning against father: 1/1000 champion: 1/5: seq1:9/25000 seq2:1999/5000000 best:seq2
  Prob winning against father: 1/1000 champion: 2/5: seq1:2/3125 seq2:1999/2500000 best:seq2
  Prob winning against father: 1/1000 champion: 1/2: seq1:3/4000 seq2:1999/2000000 best:seq2
  Prob winning against father: 1/1000 champion: 3/5: seq1:21/25000 seq2:5997/5000000 best:seq2
  Prob winning against father: 1/1000 champion: 4/5: seq1:3/3125 seq2:1999/1250000 best:seq2
  Prob winning against father: 1/1000 champion: 99/100: seq1:9999/10000000 seq2:197901/100000000 best:seq2
  Prob winning against father: 1/1000 champion: 1: seq1:1/1000 seq2:1999/1000000 best:seq2
  Prob winning against father: 1/5 champion: 2/5: seq1:16/125 seq2:18/125 best:seq2
  Prob winning against father: 1/5 champion: 1/2: seq1:3/20 seq2:9/50 best:seq2
  Prob winning against father: 1/5 champion: 3/5: seq1:21/125 seq2:27/125 best:seq2
  Prob winning against father: 1/5 champion: 4/5: seq1:24/125 seq2:36/125 best:seq2
  Prob winning against father: 1/5 champion: 99/100: seq1:9999/50000 seq2:891/2500 best:seq2
  Prob winning against father: 1/5 champion: 1: seq1:1/5 seq2:9/25 best:seq2
  Prob winning against father: 2/5 champion: 1/2: seq1:3/10 seq2:8/25 best:seq2
  Prob winning against father: 2/5 champion: 3/5: seq1:42/125 seq2:48/125 best:seq2
  Prob winning against father: 2/5 champion: 4/5: seq1:48/125 seq2:64/125 best:seq2
  Prob winning against father: 2/5 champion: 99/100: seq1:9999/25000 seq2:396/625 best:seq2
  Prob winning against father: 2/5 champion: 1: seq1:2/5 seq2:16/25 best:seq2
  Prob winning against father: 1/2 champion: 3/5: seq1:21/50 seq2:9/20 best:seq2
  Prob winning against father: 1/2 champion: 4/5: seq1:12/25 seq2:3/5 best:seq2
  Prob winning against father: 1/2 champion: 99/100: seq1:9999/20000 seq2:297/400 best:seq2
  Prob winning against father: 1/2 champion: 1: seq1:1/2 seq2:3/4 best:seq2
  Prob winning against father: 3/5 champion: 4/5: seq1:72/125 seq2:84/125 best:seq2
  Prob winning against father: 3/5 champion: 99/100: seq1:29997/50000 seq2:2079/2500 best:seq2
  Prob winning against father: 3/5 champion: 1: seq1:3/5 seq2:21/25 best:seq2
  Prob winning against father: 4/5 champion: 99/100: seq1:9999/12500 seq2:594/625 best:seq2
  Prob winning against father: 4/5 champion: 1: seq1:4/5 seq2:24/25 best:seq2
  Prob winning against father: 99/100 champion: 1: seq1:99/100 seq2:9999/10000 best:seq2

  seq1-best:0 same:8 seq2-best:28

  For these tests, sequence 1 is never best, but sequence 2 is best in 28 of
  the cases (8 cases give the same probabilities of the two sequences).

  This, Elmer should play sequence 2 (champion father champion).


  Cf my R simulation in http://www.hakank.org/sims/simulering.html 
  (Swedish text).

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model champion father)
  (enumerate

   (define seq1 (list father champion father))
   (define seq2 (list champion father champion))

   (define matches1 (for/list ([m seq1]) (bernoulli m)))  
   (define matches2 (for/list ([m seq2]) (bernoulli m)))
   
   (define (test m)
     (match m
       [(list 1 1 _) #t]
       [(list _ 1 1) #t]
       [_ #f]))
   
   (define success1 (test matches1))
   (define success2 (test matches2))   
   
   (list success1
         success2
         matches1
         matches2         
    )
   
   )
)

(display "seq1: (father champion father) seq2: (champion father champion)")
(displayln "Probability of Elmer winning against father: 4/10 champion: 2/10")
(show-marginals (model 4/10 2/10)
                (list "success1"
                      "success2"
                      "matches1"
                      "matches2"
                 ) )

(displayln "\nTesting some different cases")
(define (get-true res)
  (match res
    [(list (list #f _) (list #t true)) true]
    [(list (list #t true) (list #f _)) true]
    [_ 0]))

(define seq1-best 0)
(define seq2-best 0)
(define seq-same 0)
(for* ([father '(0 1/1000 2/10 4/10 5/10 6/10 8/10 99/100 1)]
       [champion '(0 1/1000 2/10 4/10 5/10 6/10 8/10 99/100 1)]
       #:when (> champion father) )
  (display (format "Prob winning against father: ~a champion: ~a: " father champion))
  (let* ([mod (model father champion)]
         [seq1 (get-true (first (get-probs mod #:ix 0)))]
         [seq2 (get-true (first (get-probs mod #:ix 1)))]
         [best
          (cond
            [(> seq1 seq2) (and (set! seq1-best (add1 seq1-best)) "seq1")]
            [(> seq2 seq1) (and (set! seq2-best (add1 seq2-best)) "seq2")]
            [else (and (set! seq-same (add1 seq-same)) "same")])])
    (displayln (format "seq1:~a seq2:~a best:~a" seq1 seq2 best))

    ))
(displayln (format "\nseq1-best:~a same:~a seq2-best:~a" seq1-best seq-same seq2-best))



#|
  Random probabilities: champion is always better than the father (is less probability 
  for Elmer to win).

  var : success1
  #f: 7021783/10000000 (0.7021783)
  #t: 2978217/10000000 (0.2978217)
  mean: 2978217/10000000 (0.2978217)

  var : success2
  #f: 6345217/10000000 (0.6345217)
  #t: 3654783/10000000 (0.3654783)
  mean: 3654783/10000000 (0.3654783)

  var : best
  0: 683230166719/1000000000000 (0.683230166719)
  2: 384426433281/2000000000000 (0.1922132166405)
  1: 249113233281/2000000000000 (0.1245566166405)
  mean: 1017966099843/2000000000000 (0.5089830499215)

  var : matches1
  (1 0 1): 3045217/10000000 (0.3045217)
  (1 1 1): 2004783/10000000 (0.2004783)
  (0 0 0): 1328217/10000000 (0.1328217)
  (0 0 1): 1163283/10000000 (0.1163283)
  (1 0 0): 1163283/10000000 (0.1163283)
  (1 1 0): 486717/10000000 (0.0486717)
  (0 1 1): 486717/10000000 (0.0486717)
  (0 1 0): 321783/10000000 (0.0321783)

  var : matches2
  (0 1 0): 3045217/10000000 (0.3045217)
  (0 0 0): 2004783/10000000 (0.2004783)
  (1 1 1): 1328217/10000000 (0.1328217)
  (1 1 0): 1163283/10000000 (0.1163283)
  (0 1 1): 1163283/10000000 (0.1163283)
  (0 0 1): 486717/10000000 (0.0486717)
  (1 0 0): 486717/10000000 (0.0486717)
  (1 0 1): 321783/10000000 (0.0321783)

|#
(define (model2)
  (enumerate

   (define father (/ (random-integer 101) 100))
   (define champion (/ (random-integer 101) 100))

   ; Champion is better (lower probabilities for Elmer to win) than the father
   (observe/fail (< champion father))
   
   (define seq1 (list father champion father))
   (define seq2 (list champion father champion))

   (define matches1 (for/list ([m seq1]) (bernoulli m)))  
   (define matches2 (for/list ([m seq2]) (bernoulli m)))
   
   (define (test m)
     (match m
       [(list 1 1 _) #t]
       [(list _ 1 1) #t]
       [_ #f]))

   (define success1 (test matches1))
   (define success2 (test matches2))   

   (define best
     (cond
       [(and success1 (not success2)) 1]
       [(and (not success1) success2) 2]
       [else 0]))
  
   (list success1
         success2
         best
         matches1
         matches2         
    )
   
   )
)

(displayln "\nModel 2 Random probabilities: champion is always better than the father")
(show-marginals (model2)
                (list "success1"
                      "success2"
                      "best"
                      "matches1"
                      "matches2"
                      ))
