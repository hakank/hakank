#| 

  Orch problem in Racket Gamble.
  
  From https://cbmm.mit.edu/sites/default/files/documents/CBMM_Church_Notes.html

  These are the 5 The Battle of the Two Towers Church models.
  It's mostly Church code, with a few small tweakings:
  * Church repeat is different from Racket's repeat
  * Gamble's mh-query variant does not take num-samples lag
  * In the mh-query models, #:when has to be added.


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

(require racket)
(require "gamble_utils.rkt")

#|
  You are studying the historical Battle of the Two Towers. Suppose a-priori
  you assume Legolas, Gimli and Eowyn each took out somewhere between 0 and 19 orcs.
  How many orcs did Gimli take out?

(20 : 0.1665024630541872)
(19 : 0.15369458128078817)
(18 : 0.12512315270935961)
(16 : 0.11428571428571428)
(17 : 0.10837438423645321)
(14 : 0.06502463054187192)
(15 : 0.06403940886699508)
(13 : 0.05812807881773399)
(11 : 0.04433497536945813)
(12 : 0.03940886699507389)
(10 : 0.019704433497536946)
(8 : 0.015763546798029555)
(9 : 0.01083743842364532)
(7 : 0.007881773399014778)
(6 : 0.006896551724137931)
(mean: 16.27881773399015)
 6:   6 ### (0.006 / 0    )
 7:   7 #### (0.007 / 0.006)
 8:  15 ######## (0.015 / 0.013)
 9:  10 ##### (0.01  / 0.028)
10:  19 ########## (0.019 / 0.038)
11:  44 ##################### (0.044 / 0.057)
12:  39 ################### (0.039 / 0.101)
13:  58 ############################ (0.058 / 0.14 )
14:  65 ############################### (0.065 / 0.198)
15:  64 ############################### (0.064 / 0.263)
16: 115 ####################################################### (0.115 / 0.327)
17: 109 #################################################### (0.109 / 0.442)
18: 126 ############################################################ (0.126 / 0.551)
19: 155 ########################################################################## (0.155 / 0.677)
20: 168 ################################################################################ (0.168 / 0.832)

|#
(define (two-towers1)
  (define Legolas (random-integer 21))
  (define Gimli (random-integer 21))
  (define Eowyn (random-integer  21))
  (define total-orcs (+ Legolas Gimli Eowyn))
  (if (>= total-orcs 45) Gimli (two-towers1)))
; (hist (repeat 1000 two-towers1) "Number of Orcs Gimli Took Out, Given That Everyone Took Out More Than 45")
(displayln "two-towers1:")
(define samples1 (repeat (lambda() (two-towers1)) 1000))
(show-freq samples1)
(show-histogram samples1)



#|
  Someone tells you that together, they took out at least 45 orcs. What is
  your belief about how many orcs Gimli took out?

(19 : 0.19545903257650543)
(18 : 0.15202369200394866)
(17 : 0.14610069101678183)
(16 : 0.13919052319842054)
(15 : 0.09378084896347483)
(14 : 0.07601184600197433)
(13 : 0.06712734452122408)
(12 : 0.04244817374136229)
(11 : 0.040473840078973346)
(10 : 0.02665350444225074)
(9 : 0.013820335636722606)
(8 : 0.003948667324777887)
(7 : 0.0029615004935834156)
(mean: 15.903257650542942)
 7:   2 # (0.002 / 0    )
 8:   3 ## (0.003 / 0.002)
 9:  13 ###### (0.013 / 0.005)
10:  26 ########### (0.026 / 0.018)
11:  40 ################# (0.04  / 0.044)
12:  42 ################## (0.042 / 0.084)
13:  67 ############################ (0.067 / 0.126)
14:  76 ############################### (0.076 / 0.193)
15:  94 ####################################### (0.094 / 0.269)
16: 140 ######################################################### (0.14  / 0.363)
17: 147 ############################################################ (0.147 / 0.503)
18: 153 ############################################################### (0.153 / 0.65 )
19: 197 ################################################################################ (0.197 / 0.803)

|#
(define (two-towers2)
   (define Legolas (random-integer 20))
   (define Gimli (random-integer 20))
   (define Eowyn (random-integer  20))
   (define total (+ Legolas Gimli Eowyn))
   (if (>= total 45) Gimli (two-towers2)))
;; (hist (repeat 5000 two-towers2) "Number of orcs Gimli took out given everyone took out at least 45")
(displayln "\ntwo-towers2:")
(define samples2 (repeat (lambda() (two-towers2)) 1000))
(show-freq samples2)
(show-histogram samples2)

#|

  The function above is doing rejection sampling. There is a bulit-in function in
  Church to do this: rejection-query

two-towers3:
(19 : 0.1964461994076999)
(18 : 0.18262586377097728)
(17 : 0.14215202369200394)
(16 : 0.11056268509378085)
(15 : 0.10858835143139191)
(14 : 0.07798617966436328)
(13 : 0.059230009871668314)
(12 : 0.050345508390918066)
(11 : 0.0315893385982231)
(10 : 0.017769002961500493)
(9 : 0.012833168805528134)
(7 : 0.004935834155972359)
(8 : 0.004935834155972359)
(mean: 16.01579466929911)
 7:   4 ## (0.004 / 0    )
 8:   4 ## (0.004 / 0.004)
 9:  12 ##### (0.012 / 0.008)
10:  17 ####### (0.017 / 0.02 )
11:  31 ############# (0.031 / 0.037)
12:  50 ##################### (0.05  / 0.068)
13:  59 ######################## (0.059 / 0.118)
14:  78 ################################ (0.078 / 0.177)
15: 109 ############################################# (0.109 / 0.255)
16: 111 ############################################# (0.111 / 0.364)
17: 143 ########################################################## (0.143 / 0.475)
18: 184 ########################################################################### (0.184 / 0.618)
19: 198 ################################################################################ (0.198 / 0.802)

|#
(define (two-towers3)
  (rejection-query

   (define Legolas (random-integer 20))
   (define Gimli (random-integer 20))
   (define Eowyn (random-integer  20))
   (define total (+ Legolas Gimli Eowyn))

   Gimli
   #:when
   (>= total 45)
   )
  )

;; (hist (repeat 5000 two-towers3)
;;      "Number of orcs Gimli took out given everyone took out at least 45")
(displayln "\ntwo-towers3:")
(define samples3 (repeat (lambda() (two-towers3)) 1000))
(show-freq samples3)
(show-histogram samples3)


#|
  In many cases, rejection sampling can be extremely inefficient. A better sampling
  method is mh-query. Unlike rejection-query this returns a list of the queried items.

  hakank: Note: This version of mh-query just returns one item since it's using
          mh-sampler under the hood (i.e. in gamble_utils.rkt)

(19 : 0.192497532082922)
(18 : 0.16781836130306022)
(17 : 0.12734452122408688)
(16 : 0.12240868706811452)
(15 : 0.09970384995064166)
(14 : 0.08687068114511352)
(13 : 0.05725567620927937)
(12 : 0.054294175715695954)
(11 : 0.039486673247778874)
(10 : 0.023692003948667325)
(9 : 0.01579466929911155)
(8 : 0.009871668311944718)
(7 : 0.0029615004935834156)
(mean: 15.823297137216189)
 7:   2 # (0.002 / 0    )
 8:   9 #### (0.009 / 0.002)
 9:  15 ####### (0.015 / 0.011)
10:  23 ########## (0.023 / 0.026)
11:  39 ################# (0.039 / 0.049)
12:  54 ####################### (0.054 / 0.088)
13:  57 ######################## (0.057 / 0.142)
14:  87 #################################### (0.087 / 0.199)
15: 100 ########################################## (0.1   / 0.286)
16: 123 ################################################### (0.123 / 0.386)
17: 128 ##################################################### (0.128 / 0.509)
18: 169 ###################################################################### (0.169 / 0.637)
19: 194 ################################################################################ (0.194 / 0.806)


|#
(define (two-towers4)
  (mh-query
   1000 10

   (define Legolas (random-integer 20))
   (define Gimli (random-integer 20))
   (define Eowyn (random-integer  20))
   (define total (+ Legolas Gimli Eowyn))

   Gimli
   #:when
   (>= total 45)
   )
  )
;; (hist (two-towers4) "Number of orcs Gimli took out given everyone took out at least 45")
(displayln "\ntwo-towers4:")
(define samples4 (repeat (lambda() (two-towers4)) 1000))
(show-freq samples4)
(show-histogram samples4)

#|

  This allows us to do inference even when the conditional is rare – for instance
  if the team took out at least 55 orcs. This is not code you would want to run 
  using rejection sampling since you will so rarely satisfy the conditional.

(19 : 0.6321036889332003)
(18 : 0.28614157527417744)
(17 : 0.08175473579262213)
(mean: 18.551345962113658)
17:  81 ########### (0.081 / 0    )
18: 286 ##################################### (0.286 / 0.081)
19: 633 ################################################################################ (0.633 / 0.367)

|#
(define (two-towers5)
  (mh-query
   ; rejection-query 
   ; 1000 10

   (define Legolas (random-integer 20))
   (define Gimli (random-integer 20))
   (define Eowyn (random-integer  20))
   (define total (+ Legolas Gimli Eowyn))

   Gimli
   #:when
   (>= total 55)
   )
  )

;; (hist (two-towers5) "Number of orcs Gimli took out given everyone took out at least 55")
;; (two-towers5)
(displayln "\ntwo-towers5:")
(define samples5 (repeat (lambda() (two-towers5)) 1000))
(show-freq samples5)
(show-histogram samples5)
