#| 

  True skill (R2) Racket Gamble.

  This is a port of the R2 model TrueSkillSimple.cs
  (via the WebPPL model true_skill_simple.wppl)

  With the datafiles from R2
  - true_skill_simple_r2_players.csv
  - true_skill_simple_r2_games.csv
  
  Output from the R2 model (skills):
  ```
  [0] Mean: 107.265          skills[0]
  [0] Variance: 72.9736
  
  [1] Mean: 100.541          skills[1]
  [1] Variance: 86.9287
  
  [2] Mean: 95.7907          skills[2]
  [2] Variance: 84.9168
  ```

  This model:

var : skills 0
mean: 105.37993191702496
Min: 80.2172013791733 Mean: 105.82675841495505 Max: 137.5616027531228 Variance: 76.9839079417694 Stddev: 8.77404740936413
Credible interval (0.84): 93.13274421745078..117.4310303037852

var : skills 1
mean: 99.84428542276498
Min: 70.37419959229246 Mean: 99.80363621642644 Max: 131.5575838848248 Variance: 85.90144066296219 Stddev: 9.268303008801675
Credible interval (0.84): 88.52276247991362..113.61550736374781

var : skills 2
mean: 94.41007779988429
Min: 64.64068502800022 Mean: 94.28473001770345 Max: 125.55190794665681 Variance: 83.61031634810699 Stddev: 9.1438676908684
Credible interval (0.84): 81.98624288267973..107.54704325026009

var : performance 0v1
mean: 112.45379049610435
Min: 75.75425769132332 Mean: 113.0161814844477 Max: 171.27959549792374 Variance: 228.1879612753128 Stddev: 15.105891608088308
Credible interval (0.84): 90.50321060532116..131.33171469131247

var : performance0v2
mean: 93.05698300796443
Min: 46.009771005228 Mean: 92.98532545883165 Max: 142.1334143282219 Variance: 208.21828648982824 Stddev: 14.429770839823766
Credible interval (0.84): 75.0573700745185..114.84502020342488

var : performance1v0
mean: 107.62159611032496
Min: 63.25104339204873 Mean: 106.55598020127285 Max: 149.54277583915047 Variance: 218.48348448392636 Stddev: 14.78118684287315
Credible interval (0.84): 88.53318258370878..129.7337565020298

var : performance1v2
mean: 87.44174999202477
Min: 32.38500355419641 Mean: 87.05087391614445 Max: 137.1395118552724 Variance: 213.10322992837786 Stddev: 14.59805569000125
Credible interval (0.84): 67.13159678515889..107.88562657321845

var : performance2v0
mean: 110.93152997940473
Min: 60.67561087618499 Mean: 110.98161958175665 Max: 155.02060623045796 Variance: 217.3976404457662 Stddev: 14.744410481459278
Credible interval (0.84): 91.12117901871163..131.37508032381243

var : performance2v1
mean: 88.3957799459774
Min: 28.773709261991854 Mean: 88.93407199859097 Max: 127.81645782904295 Variance: 224.15441015548836 Stddev: 14.971787139666672
Credible interval (0.84): 68.8263787800542..110.73166562582749


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


;; Data
(define players '(0 1 2))

;; Players games[g][0] vs games[g][1]: outcome games[g][2]
;;            p1 p2 result
(define games '(
                (0 1 1) ;; player 0 won over player 1
                (1 2 1) ;; player 1 won over player 2
                (0 2 1) ;; player 0 won over player 2
                ))

(define (list-ref2 lst i j)
  (list-ref (list-ref lst i) j)
  )

(define (true-skill-simple) 
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler
   
    (define num-players (length players))
    (define num-games (length games))
    ; (define m (length (first games)))

    (define skills (mem (lambda (p) (normal 100 10))))

    (define performance (mem (lambda (g p)
      (normal (skills (list-ref2 games g p)) 15)
      )))
    

    (for ([g (range num-games)])
      (cond
        [(= (list-ref2 games g 2) 1)
         (observe/fail (> (performance g 0) (performance g 1)))]
        [(= (list-ref2 games g 2) 0)
         (observe/fail (= (performance g 0) (performance g 1)))]
        [else 
         (observe/fail (< (performance g 0) (performance g 1)))]))
    
    (list (skills 0)
          (skills 1)
          (skills 2)

          (performance 0 0) ;; game 0 player 0
          (performance 0 1) ;; game 0 player 1
          
          (performance 1 0) ;; game 1 player 0
          (performance 1 1) ;; game 1 player 1
          
          (performance 2 0) ;; game 2 player 0
          (performance 2 1) ;; game 2 player 1
          )
   
   )
  )

(show-marginals (true-skill-simple)
                (list "skills 0"
                      "skills 1"
                      "skills 2"
                      "performance 0v1"
                      "performance0v2"
                      "performance1v0"
                      "performance1v2"
                      "performance2v0"
                      "performance2v1"
                      )
                #:num-samples 1000
                ; #:truncate-output 3
                #:skip-marginals? #t
                #:show-stats? #t
                #:credible-interval 0.84
                )


  
