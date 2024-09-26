#| 

  Grades in Racket.Gamble 

  From BLOG examples/grades.blog
  """
  A standard toy example used to explain probabilistic relational 
  models (PRMs) and directed acyclic probabilistic entity-relationship 
  (DAPER) models.  This version follows Heckerman, Meek and 
  Koller (2004). 
  "Probabilistic models for relational data". Microsoft Research 
  TR 2004-30.
  """

  (The comments in quotes below are from the BLOG model.)

  var : gradeJohnCS106
  A: 0.4671447117167439
  B: 0.29598925285168437
  C: 0.1704537970521482
  D: 0.04573260604502381
  F: 0.020679632334399664

  var : gradeMaryCS106
  A: 0.4836135693215332
  B: 0.3090265486725657
  C: 0.15013274336283292
  D: 0.039398230088495606
  F: 0.017828908554572292

  var : gradeFredCS106
  B: 0.40683528719548623
  C: 0.2507585649372804
  A: 0.24935537382137846
  D: 0.06620786258522929
  F: 0.026842911460625306

  This is a port of my WebPPL model grades.wppl

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

   (define Professors '("Smith" "Jones" "Moriarty"))
   (define Students   '("John" "Mary" "Fred"))
   (define Courses    '("CS106" "Phil80" "Stat10"))
   (define grades     (vector "A" "B" "C" "D" "F"))
    
    #| 
     * """
     * Relational skeleton and evidence.
     *
     * To specify the interpretations of the non-random Boolean functions, 
     * we use the ListInterp class.  The first parameter to ListInterp is
     * the number of arguments to the function.  If the number of arguments
     * is k, then the remaining parameters are interpreted in groups of k,
     * as k-tuples for which the function returns true.  
     * 
     * Given this evidence, Mary has a high probability of getting an A in 
     * CS106, because she got an A in Phil80.  Since Fred got a C in Stat10, 
     * his expected grade in CS106 is lower than Mary's.  John got the same 
     * grade as Fred in Stat10, but has an advisor; since his advisor might 
     * be friends with one of the CS106 teachers, John has a higher 
     * probability of getting an A.  
     * """
     |#

   (defmem (Teaches p c)
       (or (and (eq? p "Smith") (eq? c "CS106"))
           (and (eq? p"Jones")  (eq? c "CS106"))
           (and (eq? p "Moriarty") (eq? c "Phil80"))
           (and (eq? p "Jones") (eq? c "Stat10"))))
    
   (define (Advises p s)
     (and (eq? p "Moriarty") (eq? s "John")))
    
   (defmem (Takes s c)
     (or (and (eq? s "John") (eq? c "Stat10"))
         (and (eq? s "John") (eq? c "CS106"))
         (and (eq? s "Mary") (eq? c "Phil80"))
         (and (eq? s "Mary") (eq? c "CS106"))
         (and (eq? s "Fred") (eq? c "Stat10"))
         (and (eq? s "Fred") (eq? c "CS106"))))

    
   #| 
     * """
     * In the DAPER paper this relation is called "Friend", but there is no 
     * attempt to ensure that it's symmetric, and ensuring that in BLOG is 
     * hard. Here we use "Likes", which doesn't imply symmetry.
     * """
   |#
   (define (Likes p1  p2)
     (if (eq? p1 p2)
         #t
         (flip 0.2)))

   (defmem (Difficulty c)
     (categorical-vw2 (vector 0.7 0.3) (vector "Easy" "Hard")))


   (defmem (Intelligence s)
     (categorical-vw2 (vector 0.2 0.6 0.2) (vector "Smart" "Average" "Weak")))

   #| 
     * """
     * If one of the teachers of course c likes an advisor of student s, 
     * then student s usually gets an A.  Otherwise the grade depends on 
     * the student's intelligence and the course's difficulty.  
     * """
   |#
   (defmem (GradeObtained s c)
     (if (and (Takes s c) 
             (> (for/sum ([p1 Professors])
                  (for/sum([p2 Professors])
                    (boolean->integer (and (Teaches p1 c) (Advises p2 s) (Likes p1 p2))))) 0 ))
             (categorical-vw2 (vector 0.85 0.1 0.03 0.01 0.01) grades)
             (let ([IntelligenceS (Intelligence s)]
                   [DifficultyS (Difficulty c)])
               (if (Takes s c)
                   (cond
                     [(and (eq? IntelligenceS "Weak")    (eq? DifficultyS "Easy")) (categorical-vw2 (vector 0.2  0.4  0.3  0.07  0.03) grades)]
                     [(and (eq? IntelligenceS "Weak")    (eq? DifficultyS "Hard")) (categorical-vw2 (vector 0.05  0.1  0.55  0.2  0.1) grades)]
                     [(and (eq? IntelligenceS "Average") (eq? DifficultyS "Easy")) (categorical-vw2 (vector 0.3  0.55  0.10  0.04  0.01) grades)]
                     [(and (eq? IntelligenceS "Average") (eq? DifficultyS "Hard")) (categorical-vw2 (vector 0.15  0.3  0.45  0.07  0.03) grades)]
                     [(and (eq? IntelligenceS "Smart")   (eq? DifficultyS "Easy")) (categorical-vw2 (vector 0.85  0.1  0.03  0.01  0.01) grades)]
                     [(and (eq? IntelligenceS "Smart")   (eq? DifficultyS "Hard")) (categorical-vw2 (vector 0.60  0.25  0.1  0.03  0.02) grades)]
                     [else "unknown grade obtained"])
                   "undefined grade")
               )
             )
         )
   
   ;; Evidence
   (observe/fail (eq? (GradeObtained "John" "Stat10") "C"))
   (observe/fail (eq? (GradeObtained "Mary" "Phil80") "A"))
   (observe/fail (eq? (GradeObtained "Fred" "Stat10") "C"))
   
   (list 
    (GradeObtained "John" "CS106")
    (GradeObtained "Mary" "CS106")
    (GradeObtained "Fred" "CS106")
    )
   
   )
)

(show-marginals (model)
              (list "gradeJohnCS106"
                    "gradeMaryCS106"
                    "gradeFredCS106"

                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.9
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
