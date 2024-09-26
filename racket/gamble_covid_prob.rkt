#| 

  Covid prob in Racket Gamble.

  From Howie Hua https://twitter.com/howie_hua/status/1421502809862664197
  """
  New TikTok video: Doing my part in helping people understand the difference 
  between P(vacc|infected) and P(infected|vacc)
  """

  Population of 100 people where 90 are vaccinated and 10 are not vaccinated.
  There are 4 infections:
  - 3 that are vaccinated
  - 1 that are not vaccinated

  (n: 100 num_vaccinated: 90 total_infected: 4 infected_in_vaccinated: 3)
  var : P(vaccinated|infected)
  #t: 3/4 (0.75)
  #f: 1/4 (0.25)
  mean: 3/4 (0.75)

  var : P(infected|vaccinated)
  #f: 29/30 (0.9666666666666667)
  #t: 1/30 (0.03333333333333333)
  mean: 1/30 (0.03333333333333333)

  var : P(infected|not_vaccinated)
  #f: 9/10 (0.9)
  #t: 1/10 (0.1)
  mean: 1/10 (0.1)

  p_infected_given_vaccinated: 1/30
  p_infected_given_not_vaccinated: 1/10

  Proportion of infected for vaccinated / infected for not vaccinated:
  (P(infected|vaccinated): 1/30 P(infected|not_vaccinated): 1/10)
  proportion: 1/3

  Another way to calculate the proportion:
  proportion2: 1/3


  This is a port of my WebPPL model covid_prob.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define n 100)
(define num_vaccinated 90)
(define total_infected 4)
(define infected_in_vaccinated 3)

(show2 "n:" 100 "num_vaccinated:" num_vaccinated "total_infected:" total_infected "infected_in_vaccinated:" infected_in_vaccinated)


(define (model [var '()])

  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   ;; P(vaccinated|infected): 3/4
   (define p_vaccinated_given_infected (flip (/ infected_in_vaccinated total_infected)))
   
   ;; P(infected|vaccinated): 3/90
   (define p_infected_given_vaccinated (flip (/ infected_in_vaccinated num_vaccinated)))
   
   ;; P(infected|not vaccinated): 1/10
   (define p_infected_given_not_vaccinated (flip (/
                                                  (- total_infected infected_in_vaccinated)
                                                  (- n num_vaccinated))))
   
   (cond
     [(empty? var)
      (list p_vaccinated_given_infected
            p_infected_given_vaccinated
            p_infected_given_not_vaccinated
            )]
     [(eq? var"p_vaccinated_given_infected") p_vaccinated_given_infected]
     [(eq? var "p_infected_given_vaccinated") p_infected_given_vaccinated]
     [(eq? var "p_infected_given_not_vaccinated") p_infected_given_not_vaccinated]
     )
   

   )
  )

(show-marginals (model)
                (list "P(vaccinated|infected)"
                      "P(infected|vaccinated)"
                      "P(infected|not_vaccinated)"
                      )
                #:num-samples 10000
                #:truncate-output 5
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                )


(define p_infected_given_vaccinated (get-probs-var (model "p_infected_given_vaccinated") #t))
(define p_infected_given_not_vaccinated  (get-probs-var (model "p_infected_given_not_vaccinated") #t))

(show "p_infected_given_vaccinated" p_infected_given_vaccinated)
(show "p_infected_given_not_vaccinated" p_infected_given_not_vaccinated)

(displayln "\nProportion of infected for vaccinated / infected for not vaccinated:")
(define prop (/ p_infected_given_vaccinated p_infected_given_not_vaccinated))

(show2 "P(infected|vaccinated):" p_infected_given_vaccinated "P(infected|not_vaccinated):" p_infected_given_not_vaccinated)

(show "proportion" prop)

(displayln "\nAnother way to calculate the proportion:")
(show "proportion2" (/ (/ infected_in_vaccinated num_vaccinated)
                         (/ (- total_infected infected_in_vaccinated)
                            (- n num_vaccinated) ))
       )
