#| 

  Actor's award in Racket.Gamble 

  From "Figaro Tutorial", page 19f
  """
  There are three classes: actors, movies, and appearances relating actors to movies.
  Whether an actor receives an award for an appearance depends on the fame of the actor 
  and the quality of the movie.
  """

  var : famous actor1
  #f: 0.8903227412436111
  #t: 0.10967725875638898
  mean: 0.10967725875638898

  var : famous actor2
  #f: 0.8690895779787347
  #t: 0.1309104220212657
  mean: 0.1309104220212657

  var : famous actor3
  #t: 1.0000000000000002
  mean: 1.0000000000000002

  var : quality movie1
  medium: 0.4932975342579091
  low: 0.2885721428854028
  high: 0.2181303228566883

  var : quality movie2
  high: 1.0000000000000002

  var : award appearance1
  #f: 0.9383238386094565
  #t: 0.061676161390543986
  mean: 0.061676161390543986

  var : award appearance2
  #f: 0.7958458593820433
  #t: 0.20415414061795698
  mean: 0.20415414061795698

  var : award appearance3
  #t: 0.7341696979914993
  #f: 0.265830302008501
  mean: 0.7341696979914993

  var : probAward appearance1
  0.01: 0.4696482099881057
  0.001: 0.25921912976114625
  0.05: 0.24381075201267588
  0.2: 0.027321908238072415
  mean: 0.02261062047789048

  var : probAward appearance2
  0.05: 0.8690895779787347
  0.2: 0.1309104220212657
  mean: 0.06963656330318987

  var : probAward appearance3
  0.2: 1.0000000000000002
  mean: 0.20000000000000007


  This is a port of my WebPPL model actors_award.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler

   (defmem (famous actor) (flip 0.1))
   
   (defmem (quality movie) (categorical-vw2 (vector 0.3 0.5 0.2) (vector "low" "medium" "high")))
   
   (defmem (probAward actor movie)
     (let* ([famous_actor (famous actor)]
            [quality_movie (quality movie)]
            [t (list quality_movie famous_actor)])
       (cond 
         [(equal? t '("low" #f)) 0.001]
         [(equal? t '("low" #t)) 0.01]
         [(equal? t '("medium" #f)) 0.01]
         [(equal? t '("medium" #t)) 0.05]
         [(equal? t '("high" #f)) 0.05]
         [(equal? t '("high" #t)) 0.2]
         [else (and (writeln "else") 0.5)])))
   
   (defmem (award actor movie) (flip (probAward actor movie)))

   (define appearance1 (list "actor1" "movie1"))
   (define appearance2 (list "actor2" "movie2"))
   (define appearance3 (list "actor3" "movie2"))
   (define appearances (list appearance1 appearance2 appearance3))
    
   (observe/fail (eq? (famous "actor3") #t))
   (observe/fail (eq? (quality "movie2") "high"))
        
   ;; Ensure that exactly one appearance gets an award.
   (observe/fail (= 1 (for/sum ([a appearances])
                        (if (eq? (apply award a) #t) 1 0))))
    
   (list (famous "actor1")
         (famous "actor2")
         (famous "actor3")
         (quality "movie1")
         (quality "movie2")
         (apply award appearance1)
         (apply award appearance2)
         (apply award appearance3)
         (apply probAward appearance1)
         (apply probAward appearance2)
         (apply probAward appearance3) 
    )

   )
)

(show-marginals (model)
              (list  "famous actor1"
                     "famous actor2"
                     "famous actor3"
                     "quality movie1"
                     "quality movie2"
                     "award appearance1"
                     "award appearance2"
                     "award appearance3"
                     "probAward appearance1"
                     "probAward appearance2"
                     "probAward appearance3"                     
                     )
              #:num-samples 1000
              #:truncate-output 5
              ; #:skip-marginals? #t
              ; #:show-stats? #t
              ; #:credible-interval 0.84
                    ; #:show-histogram? #t
              ; #:show-percentiles? #t
              )


