#| 

  Medical diagnosis Racket Gamble.

  https://edu.swi-prolog.org/mod/assign/view.php?id=249
  """
  Medical diagnosis

  Develop an expert system for medical diagnosis.

  Consider three diseases: flu, gastroenteritis and bronchitis.

  A priori, flu has probability 0.3, gastroenteritis 0 0.2, and bronchitis 0.25.

  If you have the flu, you may have the following symptons, associated with their
  probabilities (symptoms are not mutually exclusive):

    fever, 0.8
    cough 0.6
    sore throat 0.5
    headaches 0.4
    aches 0.7

  If you have gastroenteritis, you may have the following symptons, associated with their
  probabilities (symptoms are not mutually exclusive):

    diarrhea 0.8
    aches 0.7
    nausea 0.4
    fatigue 0.3

  If you have bronchitis, you may have the following symptons, associated with their probabilities
  (symptoms are not mutually exclusive):

    cough 0.8
    fatigue 0.7
    fever 0.3

  Compute the probability of each disease given that the patient has the symptoms fever and aches.

  Do the same supposing the patient adds that he also experiences fatigue.
  """

  #t: 0.9580838323353293
  #f: 0.04191616766467063
  mean: 0.9580838323353293

  var : gastroenteritis
  #f: 0.7664670658682635
  #t: 0.2335329341317364
  mean: 0.2335329341317364

  var : bronchitis
  #f: 0.7185628742514969
  #t: 0.28143712574850294
  mean: 0.28143712574850294


  This is a port of my WebPPL model medical_diagnosis.wppl

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

   #|
   Consider three diseases: flu, gastroenteritis and bronchitis.
   
   A priori, flu has probability 0.3, gastroenteritis 0.2, and bronchitis 0.25.
   
   |#
    
   (define flu (flip 0.3))
   (define gastroenteritis (flip 0.2))
   (define bronchitis (flip 0.25))
    
   #|
      If you have the flu, you may have the following symptons, associated with their
      probabilities (symptoms are not mutually exclusive):
      
      fever, 0.8
      cough 0.6
      sore throat 0.5
      headaches 0.4
      aches 0.7
    |#  
   (define fever
     (cond
       [flu (flip 0.8)]
       [bronchitis (flip 0.3)]
       [else #f]));
        
   (define cough
     [cond 
       [flu (flip 0.6)]
       [bronchitis (flip 0.8)]
       [else #f]]);
    
   (define sore_throat (if flu (flip 0.5) #f))    
   (define headaches (if flu (flip 0.4) #f))
   (define aches (if flu (flip 0.7)
                      (if gastroenteritis (flip 0.7) #f)))
    
   #|
      If you have gastroenteritis, you may have the following symptons, associated with their
      probabilities (symptoms are not mutually exclusive):
      
      diarrhea 0.8
      aches 0.7
      nausea 0.4
      fatigue 0.3
    |#
    (define diarrhea (if gastroenteritis (flip 0.8) #f))
    (define nausea (if gastroenteritis (flip 0.4) #f))    
    (define fatigue (if gastroenteritis (flip 0.3) 
                        (if bronchitis (flip 0.7) #f)))
    
    #|
      If you have bronchitis, you may have the following symptons, associated with their probabilities
      (symptoms are not mutually exclusive):
      
      cough 0.8
      fatigue 0.7
      fever 0.3
    |#
    
    ;; (when (and bronchitis (flip 0.8)) (observe/fail (eq? cough #t)))
    ;; (when (and bronchitis (flip 0.7)) (observe/fail (eq? fatigue #t)))
    ;; (when (and bronchitis (flip 0.3)) (observe/fail (eq? fever #t)))        

    (observe/fail fever)
    (observe/fail aches)

    (list flu
          gastroenteritis
          bronchitis
          )
   
   )
  )

(show-marginals (model)
                (list "flu"
                      "gastroenteritis"
                      "bronchitis"
                      )
                #:num-samples 10000
                ; #:truncate-output 4
                ; #:skip-marginals? #t
                ; #:credible-interval 0.84
                )
