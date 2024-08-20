#| 

  8 schoole problem in Racket Gamble.

  This is a port of my WebPPL model 8_schools.wppl

  Result:
var : mu
6.825404882032228: 0.07799999999999996
9.615550601095673: 0.056999999999999974
8.636933793966818: 0.056999999999999974
8.025326990570234: 0.049999999999999975
...
8.467638710190993: 0.0009999999999999996
7.278582428754299: 0.0009999999999999996
9.936973578933088: 0.0009999999999999996
9.619919755105562: 0.0009999999999999996
mean: 8.644093180603333
Min: 6.426140649452243 Mean: 9.791451159511226 Max: 11.630931559369728 Variance: 1.2848420761240165 Stddev: 1.1335087454995734
ix: 129
Credible interval (0.84): 8.756284681504829..11.578941056258465
Histogram:
 6.426: 5  
 6.686: 0  
 6.947: 25 
 7.207: 3  
 7.467: 0  
 7.727: 18 
 7.988: 18 
 8.248: 45 
 8.508: 15 
 8.768: 40 
 9.029: 55 
 9.289: 76 
 9.549: 150
 9.809: 16 
10.069: 117
 10.33: 53 
 10.59: 54 
 10.85: 138
 11.11: 52 
11.371: 30 

var : tau
8.839544964890012: 0.06799999999999998
7.907030779182794: 0.05399999999999997
9.388468554018367: 0.05099999999999998
9.145862057135862: 0.04599999999999998
...
10.361651409240077: 0.0029999999999999988
10.416087376197279: 0.0029999999999999988
10.561419431593983: 0.001999999999999999
9.567393602313448: 0.001999999999999999
mean: 9.29508651175995
Min: 7.052609991126763 Mean: 9.955755087969791 Max: 13.615312602469253 Variance: 2.1726151301925953 Stddev: 1.4739793520238318
ix: 59
Credible interval (0.84): 8.039311099517548..11.352656046712683
Histogram:
 7.053: 6  
 7.381: 53 
 7.709: 0  
 8.037: 0  
 8.365: 62 
 8.693: 43 
 9.021: 49 
  9.35: 84 
 9.678: 160
10.006: 155
10.334: 4  
10.662: 176
 10.99: 59 
11.318: 32 
11.647: 20 
11.975: 0  
12.303: 11 
12.631: 0  
12.959: 3  
13.287: 21 


  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")

(define *ys*     '(28.0  8.0 -3.0  7.0 -1.0  1.0 18.0 12.0))
(define *sigmas* '(15.0 10.0 16.0 11.0  9.0 11.0 10.0 18.0))

(define (8-schools)
  
  (; rejection-sampler
   ; importance-sampler
   mh-sampler

   (define mu (normal 10 (sqrt 2)))
   
   (define tau (normal 10 (sqrt 2))) ;; > 0!  
   (observe/fail (> tau 0))
   
   (define (eta i) (normal 0 1))
   
   (define (theta i) (+ mu  (* tau (eta i))))

   (define (y i) (normal-dist (theta i) (list-ref *sigmas* i))) ; avg_likelihood 
   
   ; observe the values in y
   (for ([i (range (length *ys*))])
     ; (observe (sample (y i)) (list-ref *ys* i))
     (observe-sample (y i) (list-ref *ys* i))     
     )

   ;; (case show-var
   ;;   [("tau") tau]
   ;;   [("mu")  mu]
   ;;   )
   (list tau mu)
   )
  )

;; (for ([var '("mu" "tau")])
;;   (show "var" var)
;;   (let* ([model (8-schools var)])
;;     (show-model model #:no-dist? #t)
;;     (newline)
;;     )
;;   )

;; (newline)

(show-marginals (8-schools)
                '("mu" "tau")
                #:truncate-output 4
                #:show-stats? #t
                #:credible-interval 0.84
                #:show-histogram? #t
                )
