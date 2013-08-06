/* 

  Generative gramming (swedish) in Pop-11.

  This program generates some swedish sentences given a grammar
  and lexicon.

  Inspiration from TEACH GRAMMAR.
  
  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
uses grammar;

;;; declare a variable mygram, and initialise it with a list of
;;; rules for sentence components
vars swe_mygram =
    [
        ;;; start a list of rules
        ;;; a sentence is a NP then a VP
        [s [np vp]]
        [np 
            [snp_n] [snp_t] [snp_n pp] [snp_t pp]
         ]

        [snp_n 
          [det_n noun_n] [det_n adj_n noun_n] [det_n adj_n adj_aux_n noun_n]
        ]  

        [snp_t 
             [det_t noun_t] [det_t adj_t noun_t] [det_t adj_t adj_aux_t noun_t]
        ] 

        [pp  
           [prep snp_t] [prep snp_n]
        ]

        [vp 
            [verb np] [verb '-' verb_aux '-' np]
        ]
    ];

vars swe_mylex =
     [ ;;; start a list of lexical categories
        [noun_n  stad 
                 man 
                 kvinna
                 pojke
                 flicka 
                 dator 
                 kopp 
                 bil 
                 klocka 
                 sandstrand 
                 buske 
                 siffra
                 telefon]
        [noun_t  hus 
                 nummer 
                 krig 
                 rum 
                 träd 
                 handsfack 
                 möte ]
        [verb    hatade
                 slog 
                 kysste 
                 retade 
                 [spelade med] 
                 [gifte sig med] 
                 lärde 
                 adderade 
                 mejlade 
                 [grävde ned] 
                 [lade sig till med] 
                 borstade 
                 [stannade upp för] 
                 [väntade på] ]
        [prep    i 
                 på 
                 över 
                 under 
                 bredvid 
                 utanför innanför]
        [det_n   en]
        [det_t   ett]
        [adj_n   vacker 
                 planerad 
                 snygg 
                 sanslös 
                 svag 
                 hungrig 
                 fet 
                 smal
                 cool 
                 annan
                 dyr]
        [adj_t   vackert 
                 planerat 
                 snyggt 
                 sanslöst 
                 svagt 
                 hungrigt 
                 fett 
                 smalt
                 coolt 
                 annat
                 dyrt] 
        [adj_aux_n [men ändå snygg] 
                   [fastän trött] 
                   [och vuxen] 
                   [och lite barnslig] ]
        [adj_aux_t [men ändå snyggt] 
                   [fastän trött] 
                   [och vuxet] 
                   [och lite barnsligt] ]
        [verb_aux [med frenetisk energi] ;;; known phrases
                  [på ett snyggt sätt] 
                  [utan själ] 
                  [enbart för pengarnas skull] 
                  [utan personlig vinning] 
                  [utan vett och sans]]
     ];


;;; donouns completes unknown nouns
true -> donouns;

setup(swe_mygram, swe_mylex);

repeat 20 times 
  generate(swe_mygram, swe_mylex)->x; 
  s(x)==>; 
  x==>;
  x.flatten==>; 
  '\n\n'=>;
endrepeat;


;;; uses showtree;
;;; showtree([[en dyr
;;;                  [och vuxen]
;;;                  klocka på ett fett
;;;                  [men ändå snyggt]
;;;                  möte
;;;                  [spelade med]
;;;                  - [med frenetisk energi] - ett krig]]);
