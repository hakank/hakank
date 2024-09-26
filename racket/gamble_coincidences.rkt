#| 

  Coincidences (Birthday 'paradox') Racket Gamble.

 
  This is a port (or at least inspired) by old simulations in R
  written in 2003 (in Swedish):
  http://www.hakank.org/sims/coincidence_simulating.html
  and the Swedish blog post "Sammanträffanden - anteckningar vid läsning 
  av Diaconis och Mosteller 'Methods for Studying Coincidences'" 
  ("Coincidences - note from a reading of Diaconis and Mosteller 'Methods for Studying Coincidences'")
  http://www.hakank.org/webblogg/archives/000216.html
  (translated)
  """
  The study of coincidences is related to cognitive illusions (which is the current interest). 
  We have bad intuition regarding coincidences which the birthday problems shows: 
  It takes about 23 person for it to be a 50% probability that two persons in this group 
  shares the birthday. Surprising? A common intuition is that it require many more people .
  ...
  Here I simulate some of the most interesting sections in Diaconis' and Mosteller's paper
  'Methods for Studying Coincidences', section "7.1 General-Purpose Models: Birthday Problems" 
  (857ff).
  """ 

  Note: This program implements the plain birthday "paradox".

  * Observing 23 people.

  var : common_birthday
  0: 0.482
  1: 0.35
  2: 0.142
  3: 0.021
  4: 0.005
  mean: 0.7169999999999999

  var : num_people
  23: 1.0
  mean: 23.0

  var : p
  #t: 0.518
  #f: 0.482
  mean: 0.518


  This is a port of my WebPPL model coincidences.wppl.
  
  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")


(define (product lst) (foldl * 1 lst))
; The theoretical probability of at least two persons having the same birthday
(define (theoretical num-days num-people)
  (- 1 (product (map (lambda (v) (/ v 365.)) (range 365 (- num-days num-people) -1))))
  )

(show2 "Theoretical p for 23 people:" (theoretical 365 23))

;; (for ([p 31]
;;       #:do [(define theo (theoretical 365 p))  (show2 "p" p "theoretical" theo)]
;;       #:break (> theo 0.5))
;;   p
;;     )

(define (model [obs_num_people 23] )

  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler ; #:transition (slice) ; #:transition (enumerative-gibbs)

   ; (define obs_num_people 23)
   (define num_days 365)

   (define max_num_people 100)
   ; (define max_num_people 30)   

   ; (define num_people (add1 (random-integer num_days)))
   (define num_people (add1 (random-integer max_num_people)))   

   (when (number? obs_num_people)
     ; (observe/fail (= num_people obs_num_people))
     (observe-sample (dist-unit num_people) obs_num_people)
     )
   
   (define x (for/list ([i num_people]) (add1 (random-integer num_days))))
   
   (define common_birthday (- num_people (length (remove-duplicates x))))
   ;; (define common_birthday (for/sum ([i num_people])
   ;;                           (if (member (list-ref x i) (drop x (add1 i))) 1 0)
   ;;                           ))  
   ;; (define common_birthday (for/sum ([i num_people])
   ;;                           (for/sum([j (range (add1 i) num_people)])
   ;;                             (boolean->integer (= (list-ref x i) (list-ref x j))))))

   (define p (> common_birthday 0))

   (list common_birthday
         num_people
         p
    )

   )
  )


(show-marginals (model)
                (list "common_birthday"
                      "num_people"
                      "p"
                      "x")
                #:num-samples 1000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.94
                ; #:credible-interval2 0.94
                ; #:show-stats? #t
                ; #:show-histogram? #t ; 10 ; #t
                ; #:show-percentiles? #t ; '(0.01 0.2 0.5 0.7 0.99)  ; #t
                ; #:show-percentiles? '(0.01 0.2 0.5 0.7 0.94 0.99 0.99999)  ; #t                
                )
