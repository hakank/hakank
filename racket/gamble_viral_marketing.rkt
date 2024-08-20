#| 

  Viral marketing in Racket Gamble.

  http://cplint.eu/example/inference/viral.swinb
  """
  Viral Marketing

  A firm is interested in marketing a new product to its customers. These are connected in a 
  social network that is known to the firm: the network represents the trust relationships 
  between customers. The firm has decided to adopt a marketing strategy that involves giving 
  the product for free to a number of its customers, in the hope that these influence the 
  other customers and entice them to buy the product. The firm wants to choose the customers 
  to which marketing is applied so that its return is maximized. 
  This involves computing the probability that the non-marketed customers will acquire the 
  product given the action to the marketed customers. 

  This viral marketing scenario is inspired by (1).

  We can model this domain with an LPAD where the predicate trust/2 encodes the links 
  between customers in the social network and predicate has/1 is true of customers that 
  possess the product, either received as a gift or bought. Predicate trust/2 is defined 
  by a number of certain facts, while predicate has/1 is defined by two rules, one 
  expressing the prior probability of a customer to buy the product and one expressing the 
  fact that if a trusted customer has the product, then there is a certain probability that 
  the trusting customer buys the product. 
  """

  Reference G. V. den Broeck, I. Thon, M. van Otterlo, L. D. Raedt,
  Dtproblog: A decision-theoretic probabilistic prolog, in: M. Fox, D. Poole (Eds.),
  24th AAAI Conference on Artificial Intelligence, AAAI’10, Atlanta, Georgia, USA,
  July 11-15, 2010, AAAI Press, 2010, pp. 1217–1222.

  """
  We want to compute the probability that customer 2 buys the product if we perform the 
  action of giving the product to customer 3. We need to use causal reasoning so we use 
  the action do(has(3)) as evidence:
  ?- prob(has(2),do(has(3)),P).
  
  P = 0.136
  
  If instead we compute the effect of the action using regular probabilistic inference, we get:
  ?- prob(has(2),has(3),P).
  P = 0.4065135474609725
  So not distinguishing seeing from doing leads to an overly optimistic estimate. 
  """

  Note: I couldn't get the recursive version to work (with/without mem), but an
  "imperative" version works, i.e. using a vector-set!. This was inspired by my 
  PSI model viral_marketing.psi.

  As usual, this means that enumerate give strange solutions, so a sampler 
  is used instead.


  var : has p1
  #f: 0.7454999999999999
  #t: 0.2545
  mean: 0.2545

  var : has p2
  #f: 0.6708000000000001
  #t: 0.3292
  mean: 0.3292

  var : has p3
  #t: 1.0
  mean: 1.0

  var : has p4
  #f: 0.5938000000000001
  #t: 0.4062
  mean: 0.4062


  One should note, however, that the probability of (has p2) is about 0.3 and not 
  about 0.1 thatthe cplint model yield using the causal reasoning with its do-operator.


  This is a port of my WebPPL model viral_marketing.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)

  (; enumerate ; strange results
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define p1 0)
   (define p2 1)
   (define p3 2)
   (define p4 3)
   (define people (list p1 p2 p3 p4))
   (define num-people (length people))

    ;; From the BLOG model
    ;; fixed Boolean trusts(Person a  Person b) =
    ;;   ListInterp(2  p2 p1 
    ;;                 p3 p1 
    ;;                 p3 p2 
    ;;                 p4 p1 
    ;;                 p4 p3);
    (define trustMatrix '((0 0 0 0)  ;; p1
                          (1 0 0 0)  ;; p2
                          (1 1 0 0)  ;; p3
                          (1 0 1 0)  ;; p4
                          ))
    
    (define trusts (lambda (a b)
                          (list-ref2d trustMatrix a b)))

    #|
    ; This does not work (as I expect), either with or without (mem); 
    ; I just loops infinitely...
    (define has (mem (lambda (p) 
                       ;; if (any(function(q) (return trusts(p,q) && has(q)),people)) (
                       ; (show "has p" p)
                       ; (if (ormap (lambda (q) (and (trusts p q) (has q))) people)
                       (if (for/or ([q people]) (and (trusts p q) (has q)))
                           (flip 0.4)
                           (flip 0.1)
                           ))))
    |#
    ; And imperative version instead
    (define has (make-vector num-people #f))
    
    (for* ([p people])
      (define c 0)
      (for ([q people])
        (when (and (trusts p q) (vector-ref has q))
          (set! c (add1 c))
          )
        )
      (if (> c 0)
          (vector-set! has p (flip 0.4))
          (vector-set! has p (flip 0.1))
          )
      )
          
  
    (observe/fail (vector-ref has p3))
    ; (observe/fail (vector-ref has p4))
    
    (list (vector-ref has p1)
          (vector-ref has p2)
          (vector-ref has p3)
          (vector-ref has p4)
          )
  
   )
  )

(show-marginals (model)
                (list "has p1"
                      "has p2"
                      "has p3"
                      "has p4"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:show-stats? #t
                )

