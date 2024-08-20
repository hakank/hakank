#| 

  Biased coin in Racket Gamble.

  From cplint: http://cplint.eu/example/inference/coin.pl
  """
  Throwing a coin with uncertainty on its fairness, from
  J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated
  disjunctions. In International Conference on Logic Programming,
  volume 3131 of LNCS, pages 195-209. Springer, 2004.
  """

  From the cplint model:
  """
  heads(Coin): 1/2; tails(Coin) : 1/2:-toss(Coin),\+biased(Coin).
  % if we toss a Coin that is not biased then it lands heads with probability 1/2
  % and tails with probability 1/2

  heads(Coin): 0.6 ; tails(Coin) : 0.4:-toss(Coin),biased(Coin).
  % if we toss a Coin that is biased then it lands heads with probability 0.6
  % % and tails with probability 0.4
  
  fair(Coin):0.9 ; biased(Coin):0.1.
  % a Coin is fair with probability 0.9 and biased with probability 0.1
  
  toss(coin).
  % coin is certainly tossed
  

  ?- prob(heads(coin),Prob).  % what is the probability that coin lands heads?
  % expected result 0.51
  
  ?- prob(tails(coin),Prob).  % what is the probability that coin lands tails?
  % expected result 0.49
  
  calculation: is fair and tail 0.9*0.5 + biased and tail 0.1*0.4 = 0.9*0.5 +  0.1*0.4 = 0.49
  
  ?- prob(heads(coin),biased(coin),Prob).
  % what is the probability that coin lands heads given the coin is biased?
  % expected result 0.6
  """

  This is a port of my WebPPL model biased_coin.wppl

  Output:

No observation
var : coin-result
head: 51/100 (0.51)
tail: 49/100 (0.49)

var : coin-type
fair: 9/10 (0.9)
biased: 1/10 (0.1)

var : coin-result=head
#t: 51/100 (0.51)
#f: 49/100 (0.49)
mean: 51/100 (0.51)

var : coin-result=tail
#f: 51/100 (0.51)
#t: 49/100 (0.49)
mean: 49/100 (0.49)


Observe biased
var : coin-result
head: 3/5 (0.6)
tail: 2/5 (0.4)

var : coin-type
biased: 1 (1.0)

var : coin-result=head
#t: 3/5 (0.6)
#f: 2/5 (0.4)
mean: 3/5 (0.6)

var : coin-result=tail
#f: 3/5 (0.6)
#t: 2/5 (0.4)
mean: 2/5 (0.4)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

(define (biased-coin experiment)
  (enumerate

   (define coin-type (categorical-vw (vector "fair" "biased") (vector 9/10 1/10)))
     
   (define coin-result
     (if (eq? coin-type "fair")
              (categorical-vw (vector "head" "tail") (vector 1/2 1/2))
              (categorical-vw (vector "head" "tail") (vector 6/10 4/10))))

   (when (eq? experiment "observe biased")
     (observe/fail (eq? coin-type "biased")))

   (list coin-result coin-type (eq? coin-result "head") (eq? coin-result "tail"))
   
   )

  )

(let ([vars (list "coin-result" "coin-type" "coin-result=head" "coin-result=tail")])
  (displayln "No observation")  
  (show-marginals (biased-coin "no observation") vars)
  (newline)
  (displayln "Observe biased")
  (show-marginals (biased-coin "observe biased") vars)
  )

      
