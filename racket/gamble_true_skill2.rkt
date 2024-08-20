#| 

  (Simple) True skill Racket Gamble.

  Example inspired by
  Johannes Borgstrom, Andrew D. Gordon, Michael Greenberg, James Margetson, and Jurgen Van Gael:
  "Measure Transformer Semantics for Bayesian Machine Learning"
  https://www.microsoft.com/en-us/research/publication/measure-transformer-semantics-for-bayesian-machine-learning-2011/?from=http%3A%2F%2Fresearch.microsoft.com%2Fpubs%2F135344%2Fmsr-tr-2011-18.pdf

  In this setup the three persons a, b and c has some skill, and they have a
  performance (of some unidentified task/action). We only observe performance, 
  but not knowing the skills.
  Here we observe that:
    - a performs better than both b and c
    - b berforms better than c.

  This is a generalization of gample_true_skill.rkt
  We only need the number of people that competes.


var : performances
(109.03722682564165 100.43045632163447 83.7204414715289): 0.0009999999999999994
(109.36515828062272 94.16108549316128 92.27496631103993): 0.0009999999999999994
...
(107.3020828132587 99.13711638751745 64.52894162415731): 0.0009999999999999994
(118.258220672675 104.05445463540431 91.69125188691912): 0.0009999999999999994

var : skills
(110.53592375984272 82.22999891815194 91.66466213270587): 0.0009999999999999994
(114.65264921631625 85.14016973604896 82.42392770832667): 0.0009999999999999994
...
(91.21385893433089 100.38853585227287 95.20852804577174): 0.0009999999999999994
(122.45360801106959 107.61483151580146 105.91889051536205): 0.0009999999999999994

var : performance 0
98.49248446646037: 0.0009999999999999994
122.82447638507928: 0.0009999999999999994
...
129.15430269173476: 0.0009999999999999994
106.28175537980816: 0.0009999999999999994
mean: 114.98763345003862

var : performance 1
99.57298941789064: 0.0009999999999999994
99.15406903533663: 0.0009999999999999994
...
111.52317783528655: 0.0009999999999999994
77.06058308169544: 0.0009999999999999994
mean: 99.90728681493249

var : performance 2
73.98136068056962: 0.0009999999999999994
96.59661619675941: 0.0009999999999999994
...
67.3657104111071: 0.0009999999999999994
75.90697019853971: 0.0009999999999999994
mean: 84.72571708512255

var : skill 0
90.7477896410804: 0.0009999999999999994
105.95160250806309: 0.0009999999999999994
...
104.3108720630387: 0.0009999999999999994
99.47579449906213: 0.0009999999999999994
mean: 104.45367690932902

var : skill 1
105.08303849352771: 0.0009999999999999994
87.17766766802248: 0.0009999999999999994
...
96.62335504365944: 0.0009999999999999994
92.63812013215507: 0.0009999999999999994
mean: 99.68300292886963

var : skill 2
100.9274012918493: 0.0009999999999999994
87.49894561911844: 0.0009999999999999994
...
94.06551584378: 0.0009999999999999994
103.9055133121199: 0.0009999999999999994
mean: 95.26509862519534

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

;;; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (true-skill2 n) 
  (; enumerate 
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define people (range n))
    
   ;; Each person has an unknown Skill and a
   ;; known performance, where the skill is
   ;; reflected in the performance (with uncertainties).
   (define skill (mem (lambda (p) (normal 100 10))))
   
   (define (all-skills) (for/list ([p people]) (skill p)))   
    
   (define performance  (mem (lambda (p) (normal (skill p) 15))))
   
   (define (all-performances) (for/list ([p people]) (performance p)))
    
   ;; Now we see that 0 is better than 1 and 2, and 2 is better than 3,...
   (for ([p (range 1 n)])
     (observe/fail (> (performance (list-ref people (sub1 p)))
                      (performance (list-ref people p))))
     )

   ; (displayln all-performances)
   
   ;; What are their performance and (underlying) skills? 
   (list 
        (all-performances)
        (all-skills)
        (performance 0)
        (performance 1)
        (performance 2)        
        (skill 0)
        (skill 1)
        (skill 2)
        )
   )
  )

; (show-model (true-skill2 3))

(show-marginals (true-skill2 3)
                (list "performances"
                      "skills"                      
                      "performance 0"
                      "performance 1"
                      "performance 2"
                      "skill 0"
                      "skill 1"
                      "skill 2"
                      )
                #:num-samples 1000
                #:truncate-output 2
                ; Ah, if we skip marginals there all-performances/-skills is not shown at all
                ; #:skip-marginals? #t 
                )


  
