/*

   Example of tparse.

   See HELP TPARSE.
   """
   This is a variant of LIB GRAMMAR which is somewhat more general in that
   it will find all the parses of a list of words corresponding to a
   non-terminal. In order to do this it makes use of the POP-11 process
   mechanism. (See HELP * PROCESS).
   """ 

   This is a swedish version, using the same grammar (approx) as in mygram.p
   using the grammar library.

   See http://www.hakank.org/poplog/swegram.p for generating swedish
   sentenses with this grammar and lexicon.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

lib tparse;

/*
;;;
;;; Original from HELP TPARSES
;;;
vars grammar lexicon;
[
 [s  [np vp]   ]
 [vp [v np] [v np prep np]    ]
 [np [pn] [det noun] [np and np] [np prep np]]
 ] -> grammar;

[
 [noun   cat dog mouse man girl boy book tree]
 [pn     fred aaron steve john sharon]
 [v      liked killed thanked bought ate put]
 [prep   on over in at under]
 [det    each every the a some]
 ] -> lexicon;
*/

;;;
;;; Swedish grammar
;;;
vars swe_grammar =
    [
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
            [verb np] [verb verb_aux np]
        ]
    ];

;;;
;;; Swedish lexicon
;;;
vars swe_lexicon =
     [ 
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
                 handskfack 
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
                   [och lite barnsligt] ]
        [verb_aux [med frenetisk energi] 
                  [på ett snyggt sätt] 
                  [utan själ] 
                  [enbart för pengarnas skull] 
                  [utan personlig vinning] 
                  [utan vett och sans]]
     ];


setup(swe_grammar,swe_lexicon);
showparses("s", [en klocka retade ett snyggt handskfack]);

;;;
;;; Note: The sentence must be given with the sublists.
;;; It can use the sentences generated from 
;;; http://www.hakank.org/poplog/mygram.p.
;;;
listparses("s", [en dyr
                 [och vuxen]
                 klocka på ett fett
                 [men ändå snyggt]
                 möte
                 [spelade med]
                 [med frenetisk energi] ett krig])==>


listparses("s", [ett svagt 
                 [fastän trött] 
                 handskfack 
                 [grävde ned] 
                 [utan vett och sans] 
                 en buske])==>
