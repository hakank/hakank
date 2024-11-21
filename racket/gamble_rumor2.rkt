#| 

  Rumor in Racket/Gamble 

  From https://math.stackexchange.com/questions/12689/probability-on-spreading-of-rumors
  """
  A little help here. Exercise 21, Ch. 2 from Feller's book reads

  In a town [of] n+1 inhabitants, a person tells a rumor to a second person, who in 
  turn repeats it to a third person, etc. At each step, the recipient of the rumor 
  is chosen at random from the n people available. Find the probability that the rumor 
  will be told r times without: 
  a) returning to the originator, 
  b) being repeated to any person. 
  Do the same problem when at each step the rumor is told by one person to a gathering of N
  randomly chosen people. (The first question is the special case N=1).
  """

  * n=10, num-steps=10
  variable : repeat-start
  10: 0.3910500000000001
  2: 0.11004000000000003
  3: 0.09934000000000001
  4: 0.08606000000000001
  5: 0.07798000000000001
  6: 0.07012
  7: 0.06120000000000001
  8: 0.05511000000000002
  9: 0.04910000000000001
  mean: 6.894640000000002

  variable : repeat-any
  4: 0.15252000000000004
  3: 0.14803000000000002
  5: 0.14323000000000002
  6: 0.13205000000000003
  7: 0.11761000000000002
  2: 0.11004000000000003
  8: 0.10413000000000001
  9: 0.09163
  10: 0.0007600000000000003
  mean: 5.271280000000002

  variable : p-start
  #t: 0.60895
  #f: 0.3910500000000001
  mean: 0.60895

  variable : p-any
  #t: 0.9992399999999999
  #f: 0.0007600000000000003
  mean: 0.9992399999999999

  variable : num-distinct
  7: 0.4125700000000001
  6: 0.27108000000000004
  8: 0.21750000000000005
  5: 0.060390000000000006
  9: 0.03352000000000001
  4: 0.0041400000000000005
  10: 0.0007600000000000003
  3: 4.000000000000001e-5
  mean: 6.882380000000001


  * n=6, num-steps=10, enumerate
  variable : repeat-start
  2: 1/5 (0.2)
  10: 65536/390625 (0.16777216)
  3: 4/25 (0.16)
  4: 16/125 (0.128)
  5: 64/625 (0.1024)
  6: 256/3125 (0.08192)
  7: 1024/15625 (0.065536)
  8: 4096/78125 (0.0524288)
  9: 16384/390625 (0.04194304)
  mean: 2081606/390625 (5.32891136)

  variable : repeat-any
  3: 78884/390625 (0.20194304)
  2: 1/5 (0.2)
  4: 65204/390625 (0.16692224)
  5: 51544/390625 (0.13195264)
  6: 40424/390625 (0.10348544)
  7: 31744/390625 (0.08126464)
  8: 24992/390625 (0.06397952)
  9: 19708/390625 (0.05045248)
  mean: 1753498/390625 (4.48895488)

  variable : p-start
  #t: 325089/390625 (0.83222784)
  #f: 65536/390625 (0.16777216)
  mean: 325089/390625 (0.83222784)

  variable : p-any
  #t: 1 (1.0)
  mean: 1 (1.0)

  variable : num-distinct
  5: 37296/78125 (0.4773888)
  6: 166824/390625 (0.42706944)
  4: 1452/15625 (0.092928)
  3: 204/78125 (0.0026112)
  2: 1/390625 (2.56e-6)
  mean: 2081606/390625 (5.32891136)

  * probability that the person that starts the rumor hear the rumor again
    num-steps=10, different n

  * n: 2
  variable : p-start
  mean: 1.0

  * n: 12
  variable : p-start
  mean: 0.5345

  * n: 22
  variable : p-start
  mean: 0.3225

  * n: 32
  variable : p-start
  mean: 0.2277

  * n: 42
  variable : p-start
  mean: 0.1835

  * n: 52
  variable : p-start
  mean: 0.1455

  * n: 62
  variable : p-start
  mean: 0.1281

  * n: 72
  variable : p-start
  mean: 0.1047

  * n: 82
  variable : p-start
  mean: 0.0937

  * n: 92
  variable : p-start
  mean: 0.0838

  * Probability that any of the peope hears the rumor again
   num-steps=10, different n

  * n: 2
  variable : p-any
  mean: 1.0

  * n: 12
  variable : p-any
  mean: 0.9915

  * n: 22
  variable : p-any
  mean: 0.86

  * n: 32
  variable : p-any
  mean: 0.7181

  * n: 42
  variable : p-any
  mean: 0.6161

  * n: 52
  variable : p-any
  mean: 0.5221

  * n: 62
  variable : p-any
  mean: 0.4533

  * n: 72
  variable : p-any
  mean: 0.4097

  * n: 82
  variable : p-any
  mean: 0.3745

  * n: 92
  variable : p-any
  mean: 0.3425

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")

(define (model n)
  (show "* n" n)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   ; (define n 10) ; size of the population
   ; (define n 6) ; size of the population   
   (define ps (range n))
   (define num-steps 10)
   (define start 0)

   (define (f a)
     (let ([len (length a)])
       (if (>= len num-steps)
           a
           ; One does not tell a rumour to themselves
           (f (append a (list (uniform-draw (remove (last a) ps))))))))


   (define a (f (list start)))
   (define len (length a))
   (define num-distinct (length (remove-duplicates a)))
   ; Time when the first person hears the rumour again.
   (define repeat-start1 (for/first ([i (range 1 num-steps)]
                             #:when (= (list-ref a i) start))
                           i))
   (define repeat-start (if (eq? repeat-start1 #f) num-steps repeat-start1))
   
   ; First time any person hear the rumour again
   (define repeat-any1
     (let ([f (filter (lambda (v) (not (eq? v #f)))
               (for/list ([i num-steps])
                 ; (show2 "i" i "ai" (list-ref a i) )
                 (for/first ([j (range (add1 i) num-steps)]
                             ; #:do [(show2 "j" j "aj" (list-ref a j) )]
                             #:when (= (list-ref a i) (list-ref a j)))
                   j)))])
       (if (empty? f) num-steps (first f))))
   
   (define repeat-any (if (eq? repeat-any1 #f) num-steps repeat-any1))

   (define p-start (< repeat-start num-steps))
   (define p-any (< repeat-any num-steps))   
  
   ; (observe/fail (not (eq? repeat-any num-steps)))
   
   (list repeat-start
         repeat-any
         p-start
         p-any
         ; num-distinct
         )
   
   )
)

; (for ([n (range 2 100 10)])
(for ([n (range 10 11)])    
  (show-marginals (model n)
                (list  "repeat-start"
                       "repeat-any"
                       "p-start"
                       "p-any"
                       "num-distinct"
                       )
                #:num-samples 100000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
)
