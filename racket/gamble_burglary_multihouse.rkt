#| 

  Burglary multihouse in Racket.Gamble 

  From BLOG example/burglary-multihouse.blog

  Model 1
  var : earthquake
  #t: 0.9764848039652172
  #f: 0.023515196034782828
  mean: 0.9764848039652172

  Model 2
  evidence: ()
  var : earthquake
  #f: 0.9980000000000001
  #t: 0.0020000000000000018
  mean: 0.0020000000000000018

  evidence: (maryhouse)
  var : earthquake
  #f: 0.938927280111589
  #t: 0.061072719888411134
  mean: 0.061072719888411134

  evidence: (maryhouse johnhouse)
  var : earthquake
  #t: 0.6785815044851308
  #f: 0.3214184955148692
  mean: 0.6785815044851308

  evidence: (maryhouse johnhouse cathyhouse)
  var : earthquake
  #t: 0.9856166180590388
  #f: 0.01438338194096127
  mean: 0.9856166180590388

  evidence: (maryhouse johnhouse cathyhouse rogerhouse)
  var : earthquake
  #t: 0.9995505908340097
  #f: 0.0004494091659904078
 mean: 0.9995505908340097


  This is a port of my WebPPL model burglary_multihouse.wppl.

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Racket page: http://www.hakank.org/racket/

|#

#lang gamble

; (require gamble/viz)
(require racket)
(require "gamble_utils.rkt")
; (require "gamble_distributions.rkt")


(define (model1)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (defmem (burglary h)  (flip 0.003))
   (define earthquake (flip 0.002))
   
   (defmem (alarm h)
     (let ([burglary_h (burglary h)])
       (cond 
         [(and (not burglary_h) (not earthquake)) (flip 0.01)]
         [(and (not burglary_h) earthquake)       (flip 0.40)]
         [(and burglary_h       (not earthquake)) (flip 0.80)]
         [(and burglary_h       earthquake)       (flip 0.90)])))
   
   (observe/fail (alarm "maryhouse"))
   (observe/fail (alarm "johnhouse"))
   (observe/fail (alarm "cathyhouse"))
   (observe/fail (not (alarm "rogerhouse")))
   
   (list earthquake
         )

   )
)

(displayln "Model 1")
(show-marginals (model1)
              (list  "earthquake"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )


(define (model2 evidence)
  (enumerate
   ; rejection-sampler
   ; importance-sampler
   ; mh-sampler
   
   (defmem (burglary h)  (flip 0.003))
   (define earthquake (flip 0.002))
   
   (defmem (alarm h)
     (let ([burglary_h (burglary h)])
       (cond 
         [(and (not burglary_h) (not earthquake)) (flip 0.01)]
         [(and (not burglary_h) earthquake)       (flip 0.40)]
         [(and burglary_h       (not earthquake)) (flip 0.80)]
         [(and burglary_h       earthquake)       (flip 0.90)])))

   (for ([e evidence]) 
     (observe/fail (alarm e))
     )
   
   (list earthquake
         )

   )
)

(displayln "\nModel 2")
[define evidence '("maryhouse" "johnhouse" "cathyhouse" "rogerhouse" "hakanhouse")]
(for ([i (range (length evidence))])
  (displayln (format "evidence: ~a" (take evidence i)))
  (show-marginals (model2 (take evidence i))
              (list  "earthquake"
                     )
                    #:num-samples 1000
                    #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
  )


