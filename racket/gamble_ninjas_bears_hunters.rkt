#| 

  Ninjas, Bears, and Hunters in Racket.Gamble 

  Port of the PSI model
  https://github.com/eth-sri/psi/blob/master/test/ninjasBearsHunters.psi

  Similar to Rock, Paper, Scissors but there are some differences:
  - At start there are certain amount of ninjas, bears, and hunters. It 
    might be different for each type.
  - In each turn, two of separare types are selected and 
    one of them is eliminated according to the elimination rules
    (see below).
  - We continue until there is just one left.
  - The elimination rules are 
     * bear kills ninja
     * hunter kills bear
     * ninja kills hunter
    
   What are the probabilities that the winner is a specific type?

   The PSI model starts with
      10 ninjas, 2 bears, and 5 hunters
   and the the exact probabilities of survival are:
    (probNinjaSurvive,probBearsSurvive,probHuntersSurvive)=
    (0.221480658306459,0.767229372971558,0.0112899687219824)

   It is - at first sight - a little surprising that the probability 
   of the ninja as the winner is so low (0.22). But since there are so many 
   ninjas, the hunters are killed quite soon which mean that there is no hunter 
   left that can kill the bears (which kills the ninjas). Thus the bears have the greatest 
   probability of survival (0.77).   

   Nomething is strange with this model compared to the PSI model (similar with the WebPPL model). 
   Here's the result (importance sampler 100000 samples):

   var : probNinjaSurvive
   #f: 0.8908
   #t: 0.1092
   mean: 0.1092

   var : probBearsSurvive
   #t: 0.8695
   #f: 0.1305
   mean: 0.8695

   var : probHuntersSurvive
   #f: 0.9787
   #t: 0.0213
   mean: 0.0213

   The probability of ninja's survival is lower than the PSI model, and the probability of
   bear's survival is higher.
 
   Also see 
    * https://en.wikipedia.org/wiki/Rock_paper_scissors
    * It seems to have been popularized by the FedEx commercial: Bear, hunter ninja.
      https://www.youtube.com/watch?v=AhFbbq0zpHY
    * (The PSI model refers to a link that's not accessible.)


  This is a port of my WebPPL model ninjas_bears_hunters.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate ; #:limit 1e-01
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define start_ninjas  10)
   (define start_bears    2)
   (define start_hunters  5)
   
   (define NINJA  "ninja")
   (define BEAR   "bear")
   (define HUNTER "hunter")
   
   (define (f num_ninjas num_bears num_hunters)
     
     (define v (list num_ninjas num_bears num_hunters))
     
     (when (or (< num_ninjas 0) (< num_bears 0) (< num_hunters 0))
       (displayln "Some negative!")
       v
       )
     
     (define s (+ num_ninjas num_bears num_hunters))
     
     ;; Skip if there are only one civilization left (i.e. > 0)
     (define count_non_zero (for/sum ([e v]) (if (> e 0) 1 0)))

     ; Convenience function for below
     (define (either-civ c1 c2 p1 p2)
       (or (and (eq? c1 p1) (eq? c2 p2))
           (and (eq? c1 p2) (eq? c2 p1)))
       )
     
     (cond
       [(< s 1) v]
       [(= count_non_zero 1) v]
       [else (let* (           
                    ;; Pick the two fighters (according to the number left of each in a)
                    ;; Pick the first
                    [a (vector (/ num_ninjas s) (/ num_bears s) (/ num_hunters s))]
                    [c1 (categorical-vw2 a (vector NINJA BEAR HUNTER))]
                    
                    ;; Pick the other fighter
                    [ps2 (cond [(eq? c1 "ninja") (vector num_bears num_hunters)]
                               [(eq? c1 "bear")  (vector num_ninjas num_hunters)]
                               [else             (vector num_ninjas num_bears)])]
                    
                    [vs2 (cond [(eq? c1 "ninja") (vector BEAR HUNTER)]
                               [(eq? c1 "bear")  (vector NINJA HUNTER)]
                               [else             (vector NINJA BEAR)])]
                    [c2 (categorical-vw2 ps2 vs2)]
                    )

               ;; there must be at least two remaining civilizations.
               (cond [(not (and (> s 1)
                                (> (+ num_ninjas num_bears) 0)
                                (> (+ num_ninjas num_hunters) 0)
                                (> (+ num_bears num_hunters) 0)))
                      v]
                     
                     [(and (> num_bears  0)
                           (> num_ninjas 0)
                           (either-civ c1 c2 NINJA BEAR))
                      ;; Bear kills ninja
                      (f (sub1 num_ninjas) num_bears num_hunters)]
                     
                     [(and (> num_ninjas  0)
                           (> num_hunters 0)
                           (either-civ c1 c2 NINJA HUNTER))
                      ;; Ninja kills hunter                 
                      (f num_ninjas num_bears (sub1 num_hunters))]
                     
                     [(and (> num_hunters 0)
                           (> num_bears  0)
                           (either-civ c1 c2 BEAR HUNTER))
                      ;; Hunter kills bear
                      (f num_ninjas (sub1 num_bears) num_hunters)]
                     
                     [else 
                      (show2 "no change:" "num_ninjas:" num_ninjas "num_bears:" num_bears "num_hunters:" num_hunters)
                      v]))]
       )
     )
       
       
   (define res (f start_ninjas start_bears start_hunters))
   (define final_ninjas  (list-ref res 0))
   (define final_bears   (list-ref res 1))
   (define final_hunters (list-ref res 2))
       
   (define probNinjaSurvive   (> final_ninjas 0))
   (define probBearsSurvive   (> final_bears 0))
   (define probHuntersSurvive (> final_hunters 0))

   (list ; final_ninjas
         ; final_bears
         ; final_hunters
         probNinjaSurvive
         probBearsSurvive
         probHuntersSurvive
    )

   )
)

(show-marginals (model)
                (list  ; "final ninjas"
                       ; "final bears"
                       ; "final hunters"
                       "probNinjaSurvive"
                       "probBearsSurvive"
                       "probHuntersSurvive"
                       )
                #:num-samples 100000
                #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )


