#| 

  Chinese restaurant process in Racket/Gamble 

  https://en.wikipedia.org/wiki/Chinese_restaurant_process
  """
  In probability theory, the Chinese restaurant process is a discrete-time stochastic process, 
  analogous to seating customers at tables in a restaurant. Imagine a restaurant with an 
  infinite number of circular tables, each with infinite capacity. Customer 1 sits at the 
  first table. The next customer either sits at the same table as customer 1, or the next 
  table. This continues, with each customer choosing to either sit at an occupied table with a 
  probability proportional to the number of customers already there (i.e., they are more likely 
  to sit at a table with many customers than few), or an unoccupied table. At time n, the 
  n customers have been partitioned among m ≤ n tables (or blocks of the partition). The results 
  of this process are exchangeable, meaning the order in which the customers sit does not affect 
  the probability of the final distribution. This property greatly simplifies a number of problems 
  in population genetics, linguistic analysis, and image recognition.

  The restaurant analogy first appeared in a 1985 write-up by David Aldous,(1) where it was 
  attributed to Jim Pitman (who additionally credits Lester Dubins).(2)

  ...

  At time n+1 the element "n+1" is either
  1. added to one of the blocks of the partition Bn, where each block is chosen with probability
     |b|/(n+1) where |b| is the size of the block (i.e. the number of elements), or
  2. added to the partition Bn as new singleton block, with probability 1/(n+1)

  ...

  The probability assigned to any particular partition (ignoring the order in which customers 
  sit around any particular table) is
￼
   Pr(Bn = B) =   prod (b in B) * (|b|-1)!
                  ------------------------   B in Pn
                        n! 

  where b is a block in the partition B and |b| is the size of b.
  [Pn is the partitions of n]

  """

  The number of partitions is factorial n so enumerate might take long for larger n.

  * For max 10 guests, using enumerate:

  variable : a
  (8 1 1): 50247623/859963392 (0.05842995581839837)
  (7 1 1 1): 4524695942269/94058496000000 (0.04810512749713752)
  (7 2 1): 224420303/6019743744 (0.037280707043997385)
  (6 2 1 1): 1063622449/32256000000 (0.03297440628100198)
  (9 1): 792697/30233088 (0.02621951816499856)
  (6 3 1): 157764437/6019743744 (0.026207832710029723)
  (5 3 1 1): 2265132833699/94058496000000 (0.024082171521209526)
  (6 1 2 1): 2843661850441/141087744000000 (0.020155271959278052)
  (6 1 1 1 1): 28994814361/1469664000000 (0.019728872967562653)
  ...
  (1 1 1 1 1 1 1 2 1): 19/32659200 (5.817656280619244e-7)
  (1 1 1 1 1 1 1 3): 1/1837080 (5.443421081281164e-7)
  (1 1 1 1 1 1 1 1 2): 1/3628800 (2.755731922398589e-7)
  (1 1 1 1 1 1 1 1 1 1): 1/3628800 (2.755731922398589e-7)

  variable : a0
  5: 289903690796190859/2032510040064000000 (0.14263333763756578)
  4: 286477153507455109/2032510040064000000 (0.14094747276054512)
  6: 16273621955827/120932352000000 (0.13456797694488734)
  3: 271607123793531427/2032510040064000000 (0.1336313811197602)
  2: 251038504924046401/2032510040064000000 (0.1235115694268067)
  1: 32621942576147017/290358577152000000 (0.11235053875839092)
  7: 13529430965953/120932352000000 (0.11187602607739738)
  8: 559648255/7739670528 (0.07230905410964801)
  9: 792697/30233088 (0.02621951816499856)
  10: 1/512 (0.001953125)
  mean: 35986266075168757/8065516032000000 (4.461743790774571)

  variable : max a
  5: 484241252950344649/2032510040064000000 (0.23824790205469135)
  4: 146673413824927709/677503346688000000 (0.2164910543127884)
  6: 162784600083431/846526464000000 (0.19229711888065792)
  7: 119756010577349/846526464000000 (0.14146753311347038)
  3: 62078459466304859/677503346688000000 (0.0916282698377472)
  8: 648592829/7739670528 (0.08380109032465523)
  9: 858233/30233088 (0.02838720940447764)
  2: 60619839525901/10585989792000000 (0.0057264214983196345)
  10: 1/512 (0.001953125)
  1: 1/3628800 (2.755731922398589e-7)
  mean: 11042671677367612097/2032510040064000000 (5.433021957923464)

  variable : len
  4: 416076080659/1119744000000 (0.3715814334874757)
  3: 63710915/214990848 (0.29634245174938795)
  5: 6160145411/31104000000 (0.1980499424832819)
  2: 242461/3359232 (0.07217750962124676)
  6: 1167514938181/22404211200000 (0.052111405653103286)
  7: 266696393/36879360000 (0.007231589512399348)
  1: 1/512 (0.001953125)
  8: 136417591/256048128000 (0.0005327810520059728)
  9: 17819/914457600 (1.9485867906833516e-5)
  10: 1/3628800 (2.755731922398589e-7)
  mean: 31291305742832333/8065516032000000 (3.8796408833215166)


  See gamble_chinese_restaurant_process.rkt for an alternative model.

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

   (define max_guests 10)
   ; (define max_guests (add1 (random-integer 10)))
   
   ; Generate the table of the next customer
   ; (or return a if all customers are placed).
   (define (f a)
     (let ([a_len (length a)])
       (if (>= (sum a) max_guests)
           a
           (if (flip (/ 1 (add1 a_len)))
               ; Pick a new empty table
               (f (append a (list 1)))
               ; else: Pick a table proportion to the people already sitting there               
               (let* ([t (categorical-vw2 (list->vector a) (list->vector (range a_len)))]
                      [new_a (for/list ([i a_len]) (if (= i t) (add1 (list-ref a i)) (list-ref a i)))])
                 (f new_a))))))

    (define a (f (list 1)))
    
    (list a
          (list-ref a 0)
          (max-list a)
          (length a)
          )
    
   )
)

(show-marginals (model)
                (list  "a"
                       "a0"
                       "max a"
                       "len"
                     )
                #:num-samples 10000
                ; #:truncate-output 5
                ; #:skip-marginals? #t
                ; #:show-stats? #t
                ; #:credible-interval 0.84
                ; #:hpd-interval (list 0.84)
                ; #:show-histogram? #t
                ; #:show-percentiles? #t
                ; #:burn 0
                ; #:thin 0
                )
