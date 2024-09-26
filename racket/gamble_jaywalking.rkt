#| 

  Jaywalking in Racket.Gamble 

  This model simulates how many people (and why) that will jaywalk
  of a crossing line with red light, given that they have some
  prospensity of jaywalk if some <limit> number of people already
  has jaywalked .

  The model:
     1) Assign some number n (might be fixed)
     2) Foreach agent: generate a random tendency to jaywalk,
        here implemented as the number of people that has 
        already jaywalked for this person also to jaywalk.
        If this number is 0, then the person will jaywalk 
        no matter what.
     3) For each time slot t=0..n-1
          foreach a=agent (0..n-1): 
            if there are more than or equal number of <limit a> number 
            of people that has already jaywalked, then a will also
            jaywalk.

  The number of people that - eventually - jaywalks depends very
  much on the distribution of the limits and espespecially 
  the probability of the 0 limits jaywalkers, i.e. those that always
  jaywalk.

  For n=5 people with max-time 15, (random-integer 5) as limit distributon,
  and observing at least one person that jaywalked directly (time 0),
  enumerate can solve this in reasonable time (about 5s):

var : n
5: 2101/7221 (0.2909569311729677)
4: 615/2407 (0.2555047777316161)
3: 1525/7221 (0.2111895859299266)
2: 375/2407 (0.15579559617781472)
1: 625/7221 (0.08655310898767483)
mean: 8445/2407 (3.5085168259243873)

var : real-end-time
1: 1177/2407 (0.48899044453676777)
2: 1840/7221 (0.2548123528597147)
3: 1190/7221 (0.1647971195125329)
4: 180/2407 (0.07478188616535106)
5: 40/2407 (0.016618196925633568)
mean: 13541/7221 (1.875225038083368)

var : t 0
1: 1795/2407 (0.7457415870378064)
2: 515/2407 (0.2139592854175322)
3: 265/7221 (0.036698518210774134)
4: 25/7221 (0.0034621243595069936)
5: 1/7221 (0.00013848497438027975)
mean: 3125/2407 (1.2982966348151226)

var : t max-time
1: 2995/7221 (0.4147624982689378)
2: 475/2407 (0.19734108849189863)
5: 432/2407 (0.17947652679684253)
3: 880/7221 (0.12186677745464618)
4: 625/7221 (0.08655310898767483)
mean: 17465/7221 (2.4186400775515855)

var : steps
(1): 2995/7221 (0.4147624982689378)
(1 2): 950/7221 (0.13156072566126575)
(2): 475/7221 (0.06578036283063288)
(2 3): 110/2407 (0.045700041545492315)
(1 2 3): 110/2407 (0.045700041545492315)
(2 4 5): 80/2407 (0.033236393851267136)
(1 3): 55/2407 (0.022850020772746157)
(1 3 5): 40/2407 (0.016618196925633568)
(1 3 4): 40/2407 (0.016618196925633568)
(2 4): 40/2407 (0.016618196925633568)
(1 3 4 5): 40/2407 (0.016618196925633568)
(1 2 3 4): 40/2407 (0.016618196925633568)
(1 2 3 4 5): 40/2407 (0.016618196925633568)
(2 3 4): 40/2407 (0.016618196925633568)
(1 2 4 5): 40/2407 (0.016618196925633568)
(2 3 4 5): 40/2407 (0.016618196925633568)
(3 5): 30/2407 (0.012463647694225177)
(2 5): 80/7221 (0.01107879795042238)
(1 4 5): 20/2407 (0.008309098462816784)
(3 4): 20/2407 (0.008309098462816784)
(1 2 3 5): 20/2407 (0.008309098462816784)
(1 2 4): 20/2407 (0.008309098462816784)
(2 3 5): 20/2407 (0.008309098462816784)
(3 4 5): 20/2407 (0.008309098462816784)
(3): 55/7221 (0.007616673590915386)
(1 4): 20/7221 (0.002769699487605595)
(4 5): 20/7221 (0.002769699487605595)
(1 2 5): 20/7221 (0.002769699487605595)
(1 5): 5/7221 (0.0006924248719013987)
(4): 5/7221 (0.0006924248719013987)
(5): 1/7221 (0.00013848497438027975)

var : num-jaywalked
1: 2995/7221 (0.4147624982689378)
2: 475/2407 (0.19734108849189863)
5: 432/2407 (0.17947652679684253)
3: 880/7221 (0.12186677745464618)
4: 625/7221 (0.08655310898767483)
mean: 17465/7221 (2.4186400775515855)

var : num-not-jaywalked
0: 1107/2407 (0.459908599916909)
1: 1520/7221 (0.2104971610580252)
2: 1195/7221 (0.16548954438443428)
3: 260/2407 (0.1080182800166182)
4: 135/2407 (0.056086414624013294)
mean: 7870/7221 (1.0898767483728016)

  If we didn't do an observation on the number of jaywalkers at time 0
  (i.e. it's possible that none was jaywalking), then the average number 
  of jaywalkers goes down from 2.41 to 1.11.

var : n
mean: 3 (3.0)

var : real-end-time
mean: 13541/15625 (0.866624)

var : t 0
mean: 3/5 (0.6)

var : t max-time
mean: 3493/3125 (1.11776)

var : mean limits
mean: 2 (2.0)

var : num-jaywalked
mean: 3493/3125 (1.11776)

var : num-not-jaywalked
mean: 5882/3125 (1.88224)


  For n=6 (and at least one jaywalker at time 0), it takes about 16s
  for enumerate:
var : n
mean: 517596/124781 (4.148035357947124)

var : real-end-time
mean: 244361/124781 (1.9583189748439265)

var : t 0
mean: 163296/124781 (1.3086607736754794)

var : t max-time
mean: 323280/124781 (2.5907790448866415)

var : mean limits
mean: 234375/124781 (1.8782907654210177)

var : num-jaywalked
mean: 323280/124781 (2.5907790448866415)

var : num-not-jaywalked
mean: 194316/124781 (1.557256313060482)


  (This is based on my WebPPL model jaywalking.wppl, but it
   has changed considerably.)

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (; enumerate ; #:limit 1e-05
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define max-num-people 5) ; for enumerate 
   (define max-num-people 35)
   
   ; (define n max-num-people) ;; number of people
   (define n (add1 (random-integer max-num-people)))


   (define max-time 15)
    
   ;; The limits for jaywalking, i.e.
   ;; how many persons that have to jaywalk
   ;; before this agent jaywlk.
   (define lambda_ (add1 (random-integer max-num-people)))
   (define limits (mem (lambda() (sort (for/list ([p n])
                                         ; (poisson lambda_) ; enumerate struggle with this
                                         (random-integer max-num-people)
                          )
                        <))))
   (define (limit p) (list-ref (limits) p))
   
   ; Number of people that jaywalk no matter what
   (define t0 (for/sum ([p n]) (if (= (limit p) 0) 1 0)))
   
   ;; The number of people that will jaywalk at time t
   (defmem (step t)
     (if (= t 0)
         t0 
         (let ([step1 (step (sub1 t))])
           ; Now (step t-1) people have already jaywalked. Will p jaywalk?
           (for/sum ([p n])
             (boolean->integer (<= (limit p) step1) ) )
           )
         ))

   (define all-steps (for/list ([t max-time]) (step t)))
   
   (define real-end-time
     (if (= t0 0)
         0
         (let ([et (for/first ([t (range 1 max-time)]
                              #:when  (= (step t) (step (sub1 t))))
                     t)])
           (if et et (last all-steps)) ; checking if no end time (et) was found in the loop
         )
         )
     )
   
   (define steps (for/list ([t real-end-time]) (step t)))
   ; (define steps2 (for/list ([t max-time]) (step t)))   

   ; There is at least one jaywalker
   ; (observe/fail (> t0 0))

   ;;
   ;; Show the history
   ;;
   (define show-history #f)
   (define all-people (range n))

   (when show-history (show2 "limits" (limits) "steps" steps))
   (for ([t real-end-time])
     (if (= t 0)
         (for ([p all-people] #:when (= 0 (limit p)))
           (when show-history (show2 p "jaywalks at time 0 limit: " (limit p)))
           (set! all-people (remove p all-people))           
           )
         (for ([p all-people])
           (when (<=  (limit p) (step (sub1 t)))
             (when show-history (show2 p "jaywalks at time" t "limit: " (limit p)))
             (set! all-people (remove p all-people))
             )
           )
         )
     )
   (when show-history
     (when (> (length all-people) 0) (show "These didn't jaywalk:" all-people))
     (newline)
     (flush-output)
   )
  
   (define num-not-jaywalked (length all-people))
   (define num-jaywalked (- n num-not-jaywalked))
   
   (list n
         real-end-time
         t0
         (if (> t0 0) (step (sub1 n)) 0)
         ; (limits)
         ; (avg (limits))
         steps
         lambda_
         num-jaywalked
         num-not-jaywalked
         )

   )
)

(show-marginals (model1)
                (list  "n"
                       "real-end-time"                       
                       "t 0"
                       "t max-time"
                       ; "limits"
                       ; "mean limits"
                       "steps"
                       "lambda_"
                       "num-jaywalked"                       
                       "num-not-jaywalked"
                       )
                #:num-samples 1000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                )




