#| 

  Poker in Racket/Gamble 

  https://en.wikipedia.org/wiki/Poker_probability
  """
  Hand                Probability(%) Probability  1/Probability
  --------------------------------------------------------------------------
  Royal straight flush  0.000154%    0.00000154   ~649350.64935064935064935065
  Straight flush        0.00139%     0.0000139    ~71942.44604316546762589928    (excluding royal flush)
  Four of a kind        0.02401%     0.0002401    ~4164.93127863390254060808 
  Full House            0.1441%      0.001441     ~693.96252602359472588480
  Flush                 0.1965%      0.001965     ~508.90585241730279898219      (excluding royal flush and straight flush)
  Straight              0.3925%      0.003925     ~254.77707006369426751592      (excluding royal flush and straight flush)
  Three of a kind       2.1128%      0.021128     ~47.33055660734570238546
  Two pair              4.7539%      0.047539     ~21.03536044090115484129
  Pair                 42.2569%      0.422569     ~2.36647742735505917377
  No pair              50.1177%      0.501177     ~1.9953030566047524128
  """

  * importance-sampler (1000000 samples, 2min20s)

  variable : no_hand
  mean: 0.501332

  variable : pair
  mean: 0.422889

  variable : two_pairs
  mean: 0.047558

  variable : three_of_a_kind
  mean: 0.021083

  variable : straight
  mean: 0.003445

  variable : flush
  mean: 0.002004

  variable : full_house
  mean: 0.001457

  variable : four_of_a_kind
  mean: 0.000232

  variable : straight_flush
  mean: 1.3e-5

  variable : royal_straight_flush
  mean: 1e-6


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model)
  (; enumerate
   ; rejection-sampler
   importance-sampler
   ; mh-sampler

   (define n 52)
   (define m 5)

   (defmem (card i) (random-integer n))
   (define (value i) (modulo (card i) 13))
   (define (suite i) (modulo (card i) 4))

   (define cards (for/list ([i m]) (card i)))
   (define values (for/list ([i m]) (value i)))
   (define suites (for/list ([i m]) (suite i)))   

   ;; Ensure unicity
   (observe/fail (not (check-duplicates cards)))

   (define num_unique_values (length (remove-duplicates values)))
   (define num_unique_suites (length (remove-duplicates suites)))

   (define min_val (apply min values))
   (define max_val (apply max values))

   ; collect(values) converts to a hash map. Here we are only interested
   ; in the number of occurrences of the values.
   (define count_values (sort (hash-values (collect values)) <))

   (define pair (equal? count_values '(1 1 1 2)))

   (define three_of_a_kind (equal? count_values '(1 1 3)))

   (define two_pairs (equal? count_values '(1 2 2)))

   (define straight (and (= num_unique_values 5)
                         (= (- max_val min_val) 4)
                         (> num_unique_suites 1)))

   (define flush (and (= num_unique_suites 1) (not straight)))

   (define full_house (equal? count_values '(2 3)))

   (define four_of_a_kind (equal? count_values '(1 4)))

   (define straight_flush (and (= num_unique_suites 1)
                               (= (- max_val min_val) 4)
                               (< max_val 12)))

   (define royal_straight_flush (and (= num_unique_suites 1)
                                     (= (- max_val min_val) 4)
                                     (= max_val 12)))

   (define no_hand (and (equal? count_values '(1 1 1 1 1))
                        (not flush)
                        (not straight)))
   
   ; (show2 "values:" values "sort(values)" (sort values <))
   
   ; (observe/fail no_hand)    
   ; (observe/fail pair)
   ; (observe/fail two_pairs)
   ; (observe/fail three_of_a_kind)
   ; (observe/fail straight)    
   ; (observe/fail flush)
   ; (observe/fail four_of_a_kind)
   ; (observe/fail straight_flush)
   ; (observe/fail royal_straight_flush)
   
   ; (show2 "cards:" cards "values:" values "count_values:" count_values)
   ; (show "collect(values):" (collect_values) "count values:" count_values)
   
    
   (list no_hand
         pair
         two_pairs
         three_of_a_kind
         straight
         flush
         full_house
         four_of_a_kind
         straight_flush
         royal_straight_flush

         ;; cards
         ;; values
         ;; suites
         ;; min_val
         ;; max_val
         ;; num_unique_values
         ;; num_unique_suites
                  
         )
    
   )
)

(show-marginals (model)
              (list  "no_hand"
                     "pair"
                     "two_pairs"
                     "three_of_a_kind"
                     "straight"
                     "flush"
                     "full_house"
                     "four_of_a_kind"
                     "straight_flush"
                     "royal_straight_flush"
                     
                     "cards"
                     "values"
                     "suites"
                     "min_val"
                     "max_val"
                     "num_unique_values"
                     "num_unique_suites"
                     )
                    #:num-samples 100000 ; 1000000
                    ; #:truncate-output 5
                    #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:hpd-interval (list 0.84)
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    ; #:burn 0
                    ; #:thin 0
                    )


#|
  Showing both estimated probabilities as well as the chances.

  no_hand: 0.50132 -> 1.994733902497407  
  pair: 0.423107 -> 2.363468342523286  
  two_pairs: 0.047302 -> 21.14075514777388  
  three_of_a_kind: 0.021021 -> 47.571476142904714  
  straight: 0.003619 -> 276.3194252555955  
  flush: 0.001993 -> 501.75614651279477  
  full_house: 0.00142 -> 704.2253521126761  
  four_of_a_kind: 0.000218 -> 4587.155963302752  
  straight_flush: 1.3e-5 -> 76923.07692307692  
  royal_straight_flush: #f -> 0  


|#
(displayln "\nAnother way")
(define hands '("no_hand"
                "pair"
                "two_pairs"
                "three_of_a_kind"
                "straight"
                "flush"
                "full_house"
                "four_of_a_kind"
                "straight_flush"
                "royal_straight_flush"))

(define model-res (get-probs (model) #:num-samples 1000000))
(for ([i (length hands)])
  (let* ([prob (get-prob-value2 (list-ref model-res i) #t)]
         [inv-prob (if prob (/ 1 prob) 0)])
    (displayln (format "~a: ~a -> ~a  "(list-ref hands i) prob inv-prob))
    ))

(newline)

