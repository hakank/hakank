#| 

  Baseball hitting skills in Racket Gamble.

  Based on the PyMC3 model 
  https://www.pymc.io/projects/examples/en/latest/case_studies/hierarchical_partial_pooling.html
  """
  Hierarchical Partial Pooling

  Suppose you are tasked with estimating baseball batting skills for several players. One such 
  performance metric is batting average. Since players play a different number of games and bat 
  in different positions in the order, each player has a different number of at-bats. However, 
  you want to estimate the skill of all players, including those with a relatively small number 
  of batting opportunities.

  So, suppose a player came to bat only 4 times, and never hit the ball. Are they a bad player?

  As a disclaimer, the author of this notebook assumes little to non-existant knowledge about 
  baseball and its rules. The number of times at bat in his entire life is around “4”.
   
  Data

  We will use the baseball data for 18 players from Efron and Morris (1975).
  (https://www.swarthmore.edu/NatSci/peverso1/Sports%20Data/JamesSteinData/Efron-Morris%20Baseball/EfronMorrisBB.txt
  and it saved as EfronMorrisBB.txt.)
  """

var : phi
mean: 0.2833781058984383
Credible interval (0.84): 0.25507665333709306..0.2945030280986405

var : kappa
mean: 63.8687047273864
Credible interval (0.84): 31.26186393947033..360.6873991497251

var : theta_new
mean: 0.24110226495111683
Credible interval (0.84): 0.19627391986855042..0.2719360483892147

var : Roberto Clemente
mean: 0.17584010352770105
Credible interval (0.84): 0.2291920233371286..0.30917005645006884

var : Frank Robinson
mean: 0.25435542425927454
Credible interval (0.84): 0.2819035159537937..0.385276723395404

var : Frank Howard
mean: 0.2080445608725554
Credible interval (0.84): 0.25716551727297615..0.3726364061661014

var : Jay Johnstone
mean: 0.42063599089528236
Credible interval (0.84): 0.23744615064641036..0.2731235873744038

var : Ken Berry
mean: 0.31505076492969714
Credible interval (0.84): 0.2534683009439099..0.34213479557443677

var : Jim Spencer
mean: 0.24586529639592142
Credible interval (0.84): 0.26530209791013626..0.31984262585252915

var : Don Kessinger
mean: 0.22134398911910583
Credible interval (0.84): 0.20810906471250182..0.2764092671565338

var : Luis Alvarado
mean: 0.22864275001069867
Credible interval (0.84): 0.2076235317659351..0.2905351759982307

var : Ron Santo
mean: 0.2795693971893555
Credible interval (0.84): 0.251741561471234..0.30790003175578456

var : Ron Swaboda
mean: 0.27395678847721977
Credible interval (0.84): 0.16638298078603103..0.28407212580070723

var : Rico Petrocelli
mean: 0.30126972949033487
Credible interval (0.84): 0.15540167031366578..0.27560548939866797

var : Ellie Rodriguez
mean: 0.2703631610908696
Credible interval (0.84): 0.23621610387381406..0.3444323633293983

var : George Scott
mean: 0.29054374954168144
Credible interval (0.84): 0.2908818478185446..0.32581050386833027

var : Del Unser
mean: 0.2565371147044482
Credible interval (0.84): 0.17685898048765186..0.34344117944098984

var : Billy Williams
mean: 0.27649998721153446
Credible interval (0.84): 0.2558942289320918..0.3252197673952451

var : Bert Campaneri
mean: 0.22294674800268555
Credible interval (0.84): 0.2330639948181074..0.28337998359910355

var : Thurman Munson
mean: 0.40655152637751935
Credible interval (0.84): 0.23433134484089918..0.2860990772416365

var : Max Alvis
mean: 0.3856657365974776
Credible interval (0.84): 0.2203296398568847..0.32590990482392806

  This is a port of my WebPPL model baseball_hitting_skills.wppl 
 
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")




(define (model)

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) 
   
   (define at_bats '(45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45 45))
   (define hits    '(18 17 16 15 14 14 13 12 11 11 10 10 10 10 10  9  8  7))
   
   ;; We observe a player with 4 at bats and no hits
   (define obs_at_bats 4)
   (define obs_hits    0)
   
   (define n (length hits))
   
   (define phi (uniform 0 1))
   
   ;; Comment from the PyMC model
   ;; """
   ;; We could use pm.Pareto('kappa', m=1.5), to define our prior on kappa, but the Pareto distribution has 
   ;; very long tails. Exploring these properly is difficult for the sampler, so we use an equivalent 
   ;; but faster parametrization using the exponential distribution. We use the fact that the log of a 
   ;; Pareto distributed random variable follows an exponential distribution.
   ;; """
   (define kappa_log (exponential (/ 1.5)))
   (define kappa (exp kappa_log))
   
   ;; (define kappa (pareto 1.5 1)) ;; Testing pareto distribution.
   
   
   (define (thetas i) (beta (* phi kappa) (* (- 1.0 phi) kappa)) )
  
   ;; number of hits
   (for ([i n])
     (observe-sample (binomial-dist (list-ref at_bats i) (thetas i)) (list-ref hits i))
     ) 
   
   
   ;; Posterior
   ;; Comment from the PyMC model
   ;; """
   ;; Recall our original question was with regard to the true 
   ;; batting average for a player with only 4 at bats and no 
   ;; hits. We can add this as an additional variable in the model.
   ;; """
   (define theta_new (beta (* phi kappa) (* (- 1.0 phi) kappa)))
   
   (observe-sample (binomial-dist obs_at_bats theta_new) obs_hits) ;; number of hits
   
   (list phi
         kappa
         theta_new
         ;; Each player's theta
         (thetas 0)
         (thetas 1)
         (thetas 2)
         (thetas 3)
         (thetas 4)
         (thetas 5)
         (thetas 6)
         (thetas 7)
         (thetas 8)
         (thetas 9)
         (thetas 10)
         (thetas 11)
         (thetas 12)
         (thetas 13)
         (thetas 14)
         (thetas 15)
         (thetas 16)
         (thetas 17)
         )
        

   )
  )

(show-marginals (model)
                (list "phi"
                      "kappa"
                      "theta_new"
                      ;; Each player's theta
                      "Roberto Clemente"
                      "Frank Robinson"
                      "Frank Howard"
                      "Jay Johnstone"
                      "Ken Berry"
                      "Jim Spencer"
                      "Don Kessinger"
                      "Luis Alvarado"
                      "Ron Santo"
                      "Ron Swaboda"
                      "Rico Petrocelli"
                      "Ellie Rodriguez"
                      "George Scott"
                      "Del Unser"
                      "Billy Williams"
                      "Bert Campaneri"
                      "Thurman Munson"
                      "Max Alvis"
                      )
                #:num-samples 20000
                #:truncate-output 3
                #:skip-marginals? #t
                #:credible-interval 0.84
                ; #:credible-interval2 0.84                  
                ; #:show-stats? #t
                ; #:show-histogram? #t
                )
