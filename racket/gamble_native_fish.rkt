#| 

  Native fish in Racket Gamble.

  From
   A. Nicholson, O. Woodberry and C. Twardy
   "The 'Native Fish' Bayesian networks"
   Technical Report 2010/3, Bayesian Intelligence.
   Available from https://bayesian-intelligence.com/publications.php
   """
   A local river with tree-lined banks is known to contain native fish populations, which need
   to be conserved. Parts of the river pass through croplands, and parts are susceptible to drought
   conditions. Pesticides are known to be used on the crops. Rainfall helps native fish populations
   by maintaining water flow, which increases habitat suitability as well as connectivity between
   different habitat areas. However rain can also wash pesticides that are dangerous to fish from
   the croplands into the river. There is concern that the trees and native fish will be affected by
   drought conditions and crop pesticides.

   In short, we want to model the effect of pesticide use and rainfall on native fish abundance 
   and tree condition.
   ...
   In this model, native fish abundance has two main stressors: water-related and pesticide-related. 
   The model also has three variables describing the water-related stressor:
     - water flow and connectivity: More water keeps the river from fragmenting into ponds, and leads to faster
       flow, which washes out pollutants. Higher water levels are better for the fish.

     - rainfall: This is intended to be year-to-date rainfall, a relatively short-term indicator.

     - drought conditions: A long-term indicator intended to summarize historical conditions. A multi-year
       drought will leave the soil quite dry, so that rain which falls soaks into the ground before reach-
       ing the rivers. (For this reason, much of the rain in the Australian reservoir catchment areas has failed
       to reach the reservoirs.)

   Two variables describe the pesticide-related stressor:
    - Pesticide use: How much pesticide is being used in the river catchment.

    - Pesticide concentration in river: The amount of pesticide in the river itself – which for this example we
      imagine cannot easily be directly observed.
   """

var : native_fish_abundance
low: 0.6135750000000001
high: 0.22417500000000004
medium: 0.16225000000000006

var : tree_condition
good: 0.5750000000000003
damaged: 0.3375000000000002
dead: 0.08750000000000002

var : river_flow
poor: 0.6875000000000003
good: 0.31250000000000006

var : drought_conditions
no: 0.75
yes: 0.2500000000000001

var : annual_rainfall
below_average: 1.0000000000000002

var : pesticide_use
high: 0.9000000000000002
low: 0.10000000000000009

var : pesticide_in_river
low: 0.7200000000000008
high: 0.28000000000000014



  This is a port of my WebPPL model native_fish.wppl

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define (model)
  
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   ;; Type Level;
   (define levels (vector "high"  "medium"  "low"))
   (define highLow (vector "high"  "low"))
    
   (define conditions1 (vector "good"  "damaged"  "dead"))
   (define conditions2 (vector "good"  "poor"))
    
   (define rainfall (vector "below_average"  "average"  "above_average"))
    
   (define drought_conditions (categorical-vw2 (vector 0.25 0.75) (vector "yes" "no")))
   (define annual_rainfall    (categorical-vw2 (vector 0.1 0.7 0.2) rainfall))
   (define pesticide_use      (categorical-vw2 (vector 0.9 0.1) highLow))
   
   (define river_flow
     (cond
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "below_average")) (categorical-vw2 (vector 0.05 0.95) conditions2)]
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "average"))       (categorical-vw2 (vector 0.15 0.85) conditions2)]
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "above_average")) (categorical-vw2 (vector 0.80 0.20) conditions2)]
       [(and (eq? drought_conditions "no") (eq? annual_rainfall "below_average"))  (categorical-vw2 (vector 0.40 0.60) conditions2)]
       [(and (eq? drought_conditions "no") (eq? annual_rainfall "average"))        (categorical-vw2 (vector 0.60 0.40) conditions2)]
       [(and (eq? drought_conditions "no") (eq? annual_rainfall "above_average"))  (categorical-vw2 (vector 0.99 0.01) conditions2)]
       [else "averageXXX"]))
   
   (define pesticide_in_river
     (cond 
       [(and (eq? pesticide_use "high") (eq? annual_rainfall "below_average")) (categorical-vw2 (vector 0.3 0.7) highLow)]
       [(and (eq? pesticide_use "high") (eq? annual_rainfall "average"))       (categorical-vw2 (vector 0.6 0.4) highLow)]
       [(and (eq? pesticide_use "high") (eq? annual_rainfall "above_average")) (categorical-vw2 (vector 0.8 0.2) highLow)]
       [(and (eq? pesticide_use "low")  (eq? annual_rainfall "below_average")) (categorical-vw2 (vector 0.1 0.9) highLow)]
       [(and (eq? pesticide_use "low")  (eq? annual_rainfall "average"))       (categorical-vw2 (vector 0.2 0.8) highLow)]
       [(and (eq? pesticide_use "low")  (eq? annual_rainfall "above_average")) (categorical-vw2 (vector 0.3 0.7) highLow)]
       [else "lowXXX"]))
   
   (define native_fish_abundance
     (cond
       [(and (eq? pesticide_in_river "high") (eq? river_flow "good")) (categorical-vw2 (vector 0.2 0.4 0.4)   levels)]
       [(and (eq? pesticide_in_river "high") (eq? river_flow "poor")) (categorical-vw2 (vector 0.01 0.1 0.89) levels)]
       [(and (eq? pesticide_in_river "low")  (eq? river_flow "good")) (categorical-vw2 (vector 0.8 0.15 0.05) levels)]
       [(and (eq? pesticide_in_river "low") (eq? river_flow "poor"))  (categorical-vw2 (vector 0.05 0.15 0.8) levels)]
       [else "lowXXX"]))
   
   (define tree_condition
     (cond
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "below_average")) (categorical-vw2 (vector 0.20 0.60 0.20) conditions1)]
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "average"))       (categorical-vw2 (vector 0.25 0.60 0.15) conditions1)]
       [(and (eq? drought_conditions "yes") (eq? annual_rainfall "above_average")) (categorical-vw2 (vector 0.30 0.60 0.10) conditions1)]
       [(and (eq? drought_conditions "no")  (eq? annual_rainfall "below_average"))  (categorical-vw2 (vector 0.70 0.25 0.05) conditions1)]
       [(and (eq? drought_conditions "no")  (eq? annual_rainfall "average"))        (categorical-vw2 (vector 0.80 0.18 0.02) conditions1)]
       [(and (eq? drought_conditions "no")  (eq? annual_rainfall "above_average"))  (categorical-vw2 (vector 0.90 0.09 0.01) conditions1)]
       [else "averageXXX"]))
   
   ;; (observe/fail (eq? tree_condition "dead"))
   ;; (observe/fail (eq? pesticide_use "high"))
   
   ;; (observe/fail (eq? annual_rainfall "above_average"))
   (observe/fail (eq? annual_rainfall "below_average"))
   
   (list native_fish_abundance
         tree_condition
         river_flow
         drought_conditions
         annual_rainfall
         pesticide_use
         pesticide_in_river
         )
   
   )
  )

(show-marginals (model)
                (list "native_fish_abundance"
                      "tree_condition"
                      "river_flow"
                      "drought_conditions"
                      "annual_rainfall"
                      "pesticide_use"
                      "pesticide_in_river"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )
