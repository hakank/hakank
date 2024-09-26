#| 

  Cheating in Racket Gamble.

  The model is a port of the Cheating model (the first version)
  https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter2_MorePyMC/Ch2_MorePyMC_PyMC3.ipynb
  """
  Social data has an additional layer of interest as people are not always honest with responses, which 
  adds a further complication into inference. For example, simply asking individuals "Have you ever cheated 
  on a test?" will surely contain some rate of dishonesty. What you can say for certain is that the true 
  rate is less than your observed rate (assuming individuals lie only about not cheating; I cannot 
  imagine one who would admit "Yes" to cheating when in fact they hadn't cheated).

  To present an elegant solution to circumventing this dishonesty problem, and to demonstrate Bayesian 
  modeling, we first need to introduce the binomial distribution.

  ...

  We will use the binomial distribution to determine the frequency of students cheating during an exam. 
  If we let N be the total number of students who took the exam, and assuming each student is interviewed 
  post-exam (answering without consequence), we will receive integer X "Yes I did cheat" answers. We 
  then find the posterior distribution of p, given N, some specified prior on p, and observed data X.

  This is a completely absurd model. No student, even with a free-pass against punishment, would admit to 
  cheating. What we need is a better algorithm to ask students if they had cheated. Ideally the algorithm 
  should encourage individuals to be honest while preserving privacy. The following proposed algorithm 
  is a solution I greatly admire for its ingenuity and effectiveness:

    In the interview process for each student, the student flips a coin, hidden from the interviewer. 
    The student agrees to answer honestly if the coin comes up heads. Otherwise, if the coin comes 
    up tails, the student (secretly) flips the coin again, and answers "Yes, I did cheat" if the 
    coin flip lands heads, and "No, I did not cheat", if the coin flip lands tails. This way, the 
    interviewer does not know if a "Yes" was the result of a guilty plea, or a Heads on a second 
    coin toss. Thus privacy is preserved and the researchers receive honest answers.
  """

var : p
0.20150151218108708: 0.0004490141556466187
0.23914150422006683: 0.0004490141556466187
0.20090023935475618: 0.0004490141556466187
0.46251599611791955: 0.0004490141556466187
0.24180498656244886: 0.0004490141556466187
...
0.9906312958456832: 2.3052718313433754e-45
0.9452907311777753: 2.3052718313433754e-45
0.980524983710888: 1.5993127898018875e-48
0.8058815478867297: 2.5693096597182186e-56
0.9087178576768643: 2.653913512724549e-61
mean: 0.2332120775874492

var : obs_prop
9/25: 0.08742773173187186
7/20: 0.0862107178841507
37/100: 0.08524652827033313
33/100: 0.08046674640471957
17/50: 0.08036728465460018
...
9/10: 5.902800704927826e-42
91/100: 6.915815494030126e-45
23/25: 1.5993127898018875e-48
47/50: 2.5693096597182186e-56
19/20: 2.653913512724549e-61
mean: 0.35659278042722914


  This is a port of my WebPPL cheating.wppl

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
   ; mh-sampler ; #:transition (enumerative-gibbs)
   
   (define N 100)
   (define yes_response 35)
   
   (define p (uniform 0 1)) ; freq cheating
   
   (define (truths i) (bernoulli p))
   (define (first_flips i) (bernoulli 0.5) )
   (define (second_flips i) (bernoulli 0.5))
   
   (define val (for/list ([i N]) (+ (* (first_flips i) (truths i)) 
                                    (* (- 1 (first_flips i)) (second_flips i)))))
   (define obs_prop (/ (sum val) N))
  
   (observe-sample (binomial-dist N (exact->inexact obs_prop)) yes_response)
   ; (observe/fail (= (binomial N obs_prop) yes_response)) ; much slower

   (list p
         obs_prop
         )

   )
  )

(show-marginals (model)
                (list "p"
                      "obs_prop"
                      )
                #:num-samples 1000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t                
                )
