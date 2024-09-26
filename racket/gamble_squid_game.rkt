#| 

  Squid game in Racket.Gamble 

  From the PSI model squid.psi
  """
  tiles:=18;
  people:=16;
  def main()(
        known:=0;
        r:=();
        for i in 0..people(
                alive := true;
                while alive && known<tiles(
                        alive = (flip 1/2);
                        known += 1;
                )
                r~=(Expectation(alive));
        )
        return r; ;; expected: δ(1/262144,19/262144,43/65536,247/65536,253/16384,1577/32768,7795/65536,15751/65536,53381/131072,77691/131072,49785/65536,57741/65536,31191/32768,16131/16384,65289/65536,65493/65536)(r)
  """
  PSI model's expectation converted to float:
  0.000003814697265625, 0.000072479248046875, 0.0006561279296875,   0.0037689208984375,  0.01544189453125,    
  0.048126220703125,    0.1189422607421875,   0.2403411865234375,   0.40726470947265625, 0.5927352905273438,
  0.7596588134765625,   0.8810577392578125,   0.951873779296875,    0.98455810546875,    0.9962310791015625,   
  0.9993438720703125


  This Gamble model (enumerate):

  var : p
  (#f #f #f #f #f #f #f #f #f #t #t #t #t #t #t #t): 0.18547058105468686
  (#f #f #f #f #f #f #f #f #t #t #t #t #t #t #t #t): 0.1669235229492179
  (#f #f #f #f #f #f #f #f #f #f #t #t #t #t #t #t): 0.1669235229492179
  (#f #f #f #f #f #f #f #t #t #t #t #t #t #t #t #t): 0.12139892578124903
  (#f #f #f #f #f #f #f #f #f #f #f #t #t #t #t #t): 0.12139892578124903
  (#f #f #f #f #f #f #t #t #t #t #t #t #t #t #t #t): 0.07081604003906383
  (#f #f #f #f #f #f #f #f #f #f #f #f #t #t #t #t): 0.07081604003906383
  (#f #f #f #f #f #f #f #f #f #f #f #f #f #t #t #t): 0.03268432617187569
  (#f #f #f #f #f #t #t #t #t #t #t #t #t #t #t #t): 0.03268432617187569
  (#f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #t): 0.011672973632812722
  (#f #f #f #f #t #t #t #t #t #t #t #t #t #t #t #t): 0.011672973632812722
  (#f #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t): 0.003112792968750014
  (#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t): 0.003112792968750014
  (#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f): 0.000656127929687501
  (#f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t): 0.0005836486816406254
  (#f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t): 6.866455078125049e-5
  (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t): 3.8146972656250136e-6

  var : p 1
  #f: 0.9999961853027345
  #t: 3.8146972656250136e-6
  mean: 3.8146972656250136e-6

  var : p 2
  #f: 0.9999275207519532
  #t: 7.24792480468755e-5
  mean: 7.24792480468755e-5

  var : p 3
  #f: 0.9993438720703126
  #t: 0.0006561279296875009
  mean: 0.0006561279296875009

  var : p 4
  #f: 0.9962310791015626
  #t: 0.0037689208984375147
  mean: 0.0037689208984375147

  var : p 5
  #f: 0.9845581054687499
  #t: 0.015441894531250236
  mean: 0.015441894531250236

  var : p 6
  #f: 0.9518737792968742
  #t: 0.04812622070312592
  mean: 0.04812622070312592

  var : p 7
  #f: 0.8810577392578104
  #t: 0.11894226074218976
  mean: 0.11894226074218976

  var : p 8
  #f: 0.7596588134765614
  #t: 0.24034118652343878
  mean: 0.24034118652343878

  var : p 9
  #f: 0.5927352905273435
  #t: 0.40726470947265664
  mean: 0.40726470947265664

  var : p 18
  #t: 0.5927352905273435
  #f: 0.4072647094726567
  mean: 0.5927352905273435

  var : p 11
  #t: 0.7596588134765614
  #f: 0.24034118652343878
  mean: 0.7596588134765614

  var : p 12
  #t: 0.8810577392578104
  #f: 0.11894226074218975
  mean: 0.8810577392578104

  var : p 13
  #t: 0.9518737792968742
  #f: 0.04812622070312592
  mean: 0.9518737792968742

  var : p 14
  #t: 0.9845581054687499
  #f: 0.015441894531250236
  mean: 0.9845581054687499

  var : p 15
  #t: 0.9962310791015626
  #f: 0.0037689208984375147
  mean: 0.9962310791015626

  var : p 16
  #t: 0.9993438720703126
  #f: 0.000656127929687501
  mean: 0.9993438720703126

  var : num_survived
  7: 0.18547058105468686
  6: 0.1669235229492179
  8: 0.1669235229492179
  5: 0.12139892578124903
  9: 0.12139892578124903
  4: 0.07081604003906383
  10: 0.07081604003906383
  3: 0.03268432617187569
  11: 0.03268432617187569
  2: 0.011672973632812722
  12: 0.011672973632812722
  1: 0.003112792968750014
  13: 0.003112792968750014
  0: 0.000656127929687501
  14: 0.0005836486816406254
  15: 6.866455078125049e-5
  16: 3.8146972656250136e-6
  mean: 7.000076293945313



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

   (define num_tiles 18)
   (define num_people 16)

   (define (this_person known alive n)
      (if (= n 0)
         (list known alive)
         (if (< known num_tiles)
             (let ([alive (flip 0.5)])
               (if alive
                   (this_person (add1 known) alive (sub1 n))
                   (list (add1 known) #f)
                   ))
             (list known alive))
         )
     )

   ;; Get the successes for each person
   (define (squid p n known)
      (if (= n 0)
         p
         (let* ([ret (this_person known #t num_tiles)]
                [known2 (first ret)]
                [alive (second ret)])
           (squid (append p (list alive)) (sub1 n) known2))))

   (define p (squid '() num_people 0)) ;; did player p_(i-1) survive?
   (define num_survived (length (filter identity p))) ;; number of survived players
    
   (list p
         ;; Probability that person <i> will make it
         (list-ref p 0) ;; did person 1 make it?
         (list-ref p 1)
         (list-ref p 2)
         (list-ref p 3)
         (list-ref p 4)
         (list-ref p 5)
         (list-ref p 6)
         (list-ref p 7)
         (list-ref p 8)
         (list-ref p 9)
         (list-ref p 10)
         (list-ref p 11)
         (list-ref p 12)
         (list-ref p 13)
         (list-ref p 14)
         (list-ref p 15)
         num_survived
         )
   
   )
)

(show-marginals (model)
                (list  "p"
                       "p 1"
                       "p 2"
                       "p 3"
                       "p 4"
                       "p 5"
                       "p 6"
                       "p 7"
                       "p 8"
                       "p 9"
                       "p 18"
                       "p 11"
                       "p 12"
                       "p 13"
                       "p 14"
                       "p 15"
                       "p 16"
                       "num_survived"
                     )
                    #:num-samples 1000
                    ; #:truncate-output 5
                    ; #:skip-marginals? #t
                    ; #:show-stats? #t
                    ; #:credible-interval 0.84
                    ; #:show-histogram? #t
                    ; #:show-percentiles? #t
                    )
