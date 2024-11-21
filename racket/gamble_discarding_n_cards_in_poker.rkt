#| 

  Discarding n cards in Poker in Racket/Gamble 

  From https://discourse.julialang.org/t/optimizing-a-puzzle-from-the-balatro-game-discarding-part-of-a-poker-hand-to-get-a-pair/121912
  """
  Optimizing a puzzle from the Balatro game: discarding part of a poker hand to get a pair

  Interesting problem from Balatro, from a hand of 8 without any pairs, if you discard 
  n and draw n from the remaining deck of 44 cards. What’s the optimum n to maximize 
  your chance of obtaining at least a pair?

  I am sure many of you are obsessed by Balatro. So I wrote some code to simulate this. 
  For 1m simulations it ran in about 0.077s per n.

  Anyone wanna challenge my algorithm?
  """

  From the model, importance-sampler (100000 samples per run, )

  discard: 1
  variable : p
  mean: 0.47921

  discard: 2
  variable : p
  mean: 0.69198

  discard: 3
  variable : p
  mean: 0.80046

  discard: 4
  variable : p
  mean: 0.8524

  discard: 5
  variable : p
  mean: 0.87919

  discard: 6
  variable : p
  mean: 0.89266

  discard: 7
  variable : p
  mean: 0.89209

  discard: 8
  variable : p
  mean: 0.88051

  The exact values are (see foobar_lv2's comment), see below:
  (1 21/44 0.4772727272727273)
  (2 657/946 0.6945031712473573)
  (3 10597/13244 0.8001359106010268)
  (4 115910/135751 0.8538426972913643)
  (5 136663/155144 0.8808784097354716)
  (6 6297931/7059052 0.8921780148382531)
  (7 34182305/38320568 0.8920093512183849)
  (8 156095218/177232627 0.880736355614703)

  So, the optimal value of the number of card to discard and get is 6, with 7 as very close.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
(require "gamble_distributions.rkt")

; Values in the deck: 1..13 x 4
(define deck (flatten (repeat (lambda () (range 1 14) ) 4)))

(define (model discard)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define num-cards 8) ; cards per hand
  
   ; 0: Shuffle the deck
   (define this-deck (shuffle deck))

   ; 1: Get the hand
   (define hand (take this-deck num-cards))

   ; Ensure there's no pairs in the hand, i.e. all values are unique
   (observe/fail (= (length (remove-duplicates hand)) (length hand)))

   ; 2: Discard n card at random to get the second hand
   (define hand2 (append (drop hand discard) (take (drop this-deck num-cards) discard)))

   ; Check if there's at least one pair in the new hand
   (define tt (collect hand2))

   (define num-duplicates (for/sum ([c (hash-values tt)])
                            (b2i (> c 1))
                            ))
   
   (define p (> num-duplicates 0))
   
   (list p
         ; num-duplicates
         ; hand
         ; tt
         )

   )
)

(for ([discard (range 1 9)])
  (show "discard" discard)
(show-marginals (model discard)
                (list  "p"
                       "num-duplicates"
                       "hand"
                       "t"
                       )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
)


#|
  Exact calculation

  Julia code
  julia> bigbin(n,k)=binomial(BigInt(n), BigInt(k))
  julia> function prob(d)
       numfails = BigInt(0)
       for j=0:d
         jbar = d - j
         numfails += 4^jbar * bigbin(5, jbar) * 3^j * bigbin(d, j)
       end
        Float64(1 - numfails/bigbin(44, d))
       end
  julia> [prob(i) for i=1:8]

  Result:
  Exact calculation
(1 21/44 0.4772727272727273)
(2 657/946 0.6945031712473573)
(3 10597/13244 0.8001359106010268)
(4 115910/135751 0.8538426972913643)
(5 136663/155144 0.8808784097354716)
(6 6297931/7059052 0.8921780148382531)
(7 34182305/38320568 0.8920093512183849)
(8 156095218/177232627 0.880736355614703)


|#

(define (prob d)
  (let ([numfails (for/sum ([j (range 0 (add1 d))])
                    (let ([jbar (- d j)])
                      (* (expt 4 jbar) (binomialf 5 jbar) (expt 3 j) (binomialf d j))))
                  ])
    ; (show "numfails" numfails)
        (- 1 (/ numfails (binomialf 44 d)))
        )
  )
(displayln "\nExact calculation")
(for ([d (range 1 9)])
  (let ([p (prob d)])
    (show2 d p  (* 1.0 p))
  ))

  
