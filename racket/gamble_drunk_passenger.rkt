#| 

  Drunk passenger in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/101
  """
  A line of 100 airline passengers is waiting to board a plane. They each hold a ticket 
  to one of the 100 seats on that flight. For convenience, let's say that the nth
  passenger in line has a ticket for seat number 'n'. Being drunk, the first person in 
  line picks a random seat (equally likely for each seat). All of the other passengers 
  are sober, and will go to their assigned seats unless it is already occupied; If it 
  is occupied, they will then find a free seat to sit in, at random. What is the probability 
  that the last (100th) person to board the plane will sit in their own seat (#100)?

  ...

  Follow-up Question: What's the probability that the second-last person sits on their seat?

  """

  n=100 is too large/slow for enumerate to handle. Using importance-sampler:

  var : last-seat
  100: 0.5041
  1: 0.4959
  mean: 50.905899999999995

  var : p
  #t: 0.5041
  #f: 0.4959
  mean: 0.5041


  For n=10 we got the exact probability using enumerate:

  var : last-seat
  1: 1/2 (0.5)
  10: 1/2 (0.5)
  mean: 11/2 (5.5)

  var : p
  #f: 1/2 (0.5)
  #t: 1/2 (0.5)
  mean: 1/2 (0.5)


  Follow-up Question: What's the probability that the second-last person sits on their seat?

  For n=100
  var : next-to-last-seat
  99: 0.6599999999999999
  1: 0.1725
  100: 0.1675
  mean: 82.26249999999999

  var : p2
  #t: 0.6599999999999999
  #f: 0.33999999999999997
  mean: 0.6599999999999999


  For n=10:
  var : next-to-last-seat
  9: 2/3 (0.6666666666666666)
  1: 1/6 (0.16666666666666666)
  10: 1/6 (0.16666666666666666)
  mean: 47/6 (7.833333333333333)

  var : p2
  #t: 2/3 (0.6666666666666666)
  #f: 1/3 (0.3333333333333333)
  mean: 2/3 (0.6666666666666666)


  * For a random-passenger to find their seat:

  For n=100
  var : p3
  #t: 0.9452000000000044
  #f: 0.054800000000000536
  mean: 0.9452000000000044

  For n=10
  var : p3
  #t: 18071/25200 (0.7171031746031746)
  #f: 7129/25200 (0.2828968253968254)
  mean: 18071/25200 (0.7171031746031746)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(require racket/set)

(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (define n 10)
   
   (define (free-seats a) (set-symmetric-difference (range 1 (add1 n)) a))
   
   (define (pick-seat p a)
     (cond
       [(= (length a) n) a]         
       [else  (if (or (= p 1) (member p a)) ; first or already occupied
                  (pick-seat (add1 p) (append a (list (uniform-draw (free-seats a) ))))
                  (pick-seat (add1 p) (append a (list p)))                    
                  )]
         ))
   
   (define seats (pick-seat 1 '()))

   (define last-seat (last seats))
   (define p (= last-seat n))

   ; Follow-up question
   (define next-to-last-seat (list-ref seats (- n 2)))
   (define p2 (= next-to-last-seat (sub1 n)))

   (define random-passenger (add1 (random-integer n)))
   (define random-passenger-seat (list-ref seats (sub1 random-passenger)))
   (define p3 (= random-passenger-seat random-passenger)) 
   
   (list last-seat
         p
         next-to-last-seat
         p2
         random-passenger
         random-passenger-seat
         p3
         )
   
      
   )
)

(show-marginals (model)
                (list  "last-seat"
                       "p"
                       "next-to-last-seat"
                       "p2"
                       "random-passenger"
                       "random-passenger-seat"
                       "p3"
                       )
                #:num-samples 10000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


