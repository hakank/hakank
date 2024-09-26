#| 

  Number of double heads in Racket.Gamble 

  https://brainstellar.com/puzzles/probability/118
  """
  A coin is tossed 10 times and the output written as a string. 
  What is the expected number of HH? Note that in HHH, number of HH = 2. 
  (eg: expected number of HH in 2 tosses is 0.25, 3 tosses is 0.5)

  Solution: E[n] = (n-1)/4 ... E[10]=2.5.
  """

  var : double-heads
  2: 241/1024 (0.2353515625)
  1: 235/1024 (0.2294921875)
  3: 187/1024 (0.1826171875)
  0: 9/64 (0.140625)
  4: 29/256 (0.11328125)
  5: 31/512 (0.060546875)
  6: 25/1024 (0.0244140625)
  7: 11/1024 (0.0107421875)
  8: 1/512 (0.001953125)
  9: 1/1024 (0.0009765625)
  mean: 9/4 (2.25)

  For n=20

  var : double-heads
  4: 10803/65536 (0.1648406982421875)
  5: 162843/1048576 (0.15529918670654297)
  3: 155141/1048576 (0.14795398712158203)
  6: 16695/131072 (0.12737274169921875)
  2: 28017/262144 (0.10687637329101563)
  7: 97153/1048576 (0.0926523208618164)
  8: 31713/524288 (0.06048774719238281)
  1: 59155/1048576 (0.05641460418701172)
  9: 18749/524288 (0.03576087951660156)
  10: 10059/524288 (0.019186019897460938)
  0: 17711/1048576 (0.016890525817871094)
  11: 2475/262144 (0.009441375732421875)
  12: 547/131072 (0.00417327880859375)
  13: 1827/1048576 (0.0017423629760742188)
  14: 159/262144 (0.000606536865234375)
  15: 237/1048576 (0.00022602081298828125)
  16: 55/1048576 (5.245208740234375e-5)
  17: 21/1048576 (2.002716064453125e-5)
  18: 1/524288 (1.9073486328125e-6)
  19: 1/1048576 (9.5367431640625e-7)
  mean: 19/4 (4.75)


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model n)
  (enumerate

   (define tosses (for/list ([i n]) (bernoulli 1/2)))

   (define double-heads (for/sum ([chunk (chunks-of tosses 2)])
                          (if (equal? chunk '(1 1)) 1 0)))
   
   (list double-heads
         )
   
   )
)

(for ([n (range 2 11)])
  (show "n" n)
  (show-marginals (model n)
                (list  "double-heads"))
)

