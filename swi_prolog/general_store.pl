/*

  General store problem in SWI Prolog

  From http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html

  """
  The General Store  from "Mathematical Puzzles of Sam Loyd", number 30

  The owner of a general store, who is something of a puzzlist, has put
  up this sign to see if any of his mathematical friends can translate
  it properly. Each different letter stands for a different digit. The
  words above the horizontal line represent numbers that add to the
  total of "ALL WOOL". The problem is to change all the letters to the
  correct digits.

         C H E S S
   +       C A S H
   +   B O W W O W
   +     C H O P S
   +   A L S O P S
   + P A L E A L E
   +       C O O L
   +       B A S S
   +       H O P S
   +       A L E S
   +       H O E S
   +   A P P L E S
   +       C O W S 
   +   C H E E S E
   +   C H S O A P
   +     S H E E P
   _______________
     A L L W O O L
   """

  Here are three different models, inspired by the Oz solutions from
  the page above.

  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

/*
  Time to first solution (which happens to be unique). store1/1 is a bit faster.

  Model 1: 
  % 2,396,603 inferences, 0.152 CPU in 0.152 seconds (100% CPU, 15815015 Lips)
  [4,5,2,0,3,6,8,9,1,7]
  
  Model 2: 
  % 7,620,461 inferences, 0.496 CPU in 0.496 seconds (100% CPU, 15352908 Lips)
  [4,5,2,0,3,6,8,9,1,7]
  
  Model 3:
  % 4,830,569 inferences, 0.313 CPU in 0.313 seconds (100% CPU, 15441670 Lips)
  [3,6,4,2,5,7,8,1,0,9]

*/
go :-
        write("Model 1: "),
        time(once(store1(L1))),
        writeln(L1),

        write("Model 2: "),
        time(once(store2(L2))),
        writeln(L2),

        write("Model 3: "),
        time(once(store3(L3))),
        writeln(L3),
        nl.


/*
  
  findall is used to check for all possible solutions.
  Here store1/1 is much faster than the other two variants.

  Model 1: 
  % 11,293,175 inferences, 0.718 CPU in 0.718 seconds (100% CPU, 15737852 Lips)
  [[c=4,h=5,e=2,s=0,a=3,b=6,o=8,w=9,p=1,l=7]]
  
  Model 2: 
  % 124,168,845 inferences, 8.190 CPU in 8.190 seconds (100% CPU, 15160304 Lips)
  [[c=4,h=5,e=2,s=0,a=3,b=6,o=8,w=9,p=1,l=7]]
  
  Model 3: 
  % 93,825,390 inferences, 6.047 CPU in 6.047 seconds (100% CPU, 15516710 Lips)
  [[a=3,b=6,c=4,e=2,h=5,l=7,o=8,p=1,s=0,w=9]]

*/
go2 :-
        write("Model 1: "),
        time(findall([c=C, h=H, e=E, s=S, a=A, b=B, o=O, w=W, p=P, l=L], store1([C, H, E, S, A, B, O, W, P, L]),L1)),
        writeln(L1),

        write("Model 2: "),
        time(findall([c=C, h=H, e=E, s=S, a=A, b=B, o=O, w=W, p=P, l=L], store2([C, H, E, S, A, B, O, W, P, L]),L2)),
        writeln(L2),

        writef("Model 3: "),
        time(findall([a=A, b=B, c=C, e=E, h=H, l=L, o=O, p=P, s=S, w=W], store3([A, B, C, E, H, L, O, P, S, W]),L3)),
        writeln(L3),
        nl.


store1(LD) :-

   LD = [C, H, E, S, A, B, O, W, P, L],
   LD ins 0..9,
 
   all_different(LD),
   
   10000*C + 1000*H + 100*E + 10*S + S
   + 1000*C + 100*A + 10*S + H
   + 100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   + 10000*C + 1000*H + 100*O + 10*P + S
   + 100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   + 1000*C + 100*O + 10*O + L
   + 1000*B + 100*A + 10*S + S
   + 1000*H + 100*O + 10*P + S
   + 1000*A + 100*L + 10*E + S
   + 1000*H + 100*O + 10*E + S
   + 100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   + 1000*C + 100*O + 10*W + S
   + 100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   + 100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   + 10000*S + 1000*H + 100*E + 10*E + P
   
   #= 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L,

   labeling([ff,enum],LD).
   


store2(LD) :-
   LD=[C, H, E, S, A, B, O, W, P, L],
   LD ins 0..9,

   all_different(LD),

   Carries= [C11, C12, C21, C22, C31, C32, C41, C42, C51, C52, C61, C62],
   Carries ins 0..9,

   S+H+W+S+S+E+L+S+S+S+S+S+S+E+P+P       #= L+ 10*C11+ 100*C12,
   S+S+O+P+P+L+O+S+P+E+E+E+W+S+A+E+ C11       #= O+ 10*C21+ 100*C22,
   E+A+W+O+O+A+O+A+O+L+O+L+O+E+O+E+ C21 + C12 #= O+ 10*C31+ 100*C32,
   H+C+W+H+S+E+C+B+H+A+H+P+C+E+S+H+ C31 + C22 #= W+ 10*C41+ 100*C42,
   C+  O+C+L+L+     P+  H+H+S+ C41 + C32 #= L+ 10*C51+ 100*C52,
   B+ A+A+     A+  C+C  + C51 + C42 #= L+ 10*C61+ 100*C62,
   P          + C61 + C52 #= A,

   flatten([Carries,LD],Vars),
   labeling([ffc],Vars).



store3(LD) :-

   LD = [A, B, C, E, H, L, O, P, S, W],
   LD ins 0..9,
        
   CarriesLow = [C11, C21, C31, C41, C51, C61],
   CarriesHigh =[C12, C22, C32, C42, C52],
   CarriesLow ins 0..9,
   CarriesHigh ins 0..1,

   all_different(LD),

   
   10000*C + 1000*H + 100*E + 10*S + S
   + 1000*C + 100*A + 10*S + H
   + 100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   + 10000*C + 1000*H + 100*O + 10*P + S
   + 100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   + 1000*C + 100*O + 10*O + L
   + 1000*B + 100*A + 10*S + S
   + 1000*H + 100*O + 10*P + S
   + 1000*A + 100*L + 10*E + S
   + 1000*H + 100*O + 10*E + S
   + 100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   + 1000*C + 100*O + 10*W + S
   + 100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   + 100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   + 10000*S + 1000*H + 100*E + 10*E + P   
   #= 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L,
   
   S+H+W+S+S+E+L+S+S+S+S+S+S+E+P+P       #= L+ 10*C11+ 100*C12,
   S+S+O+P+P+L+O+S+P+E+E+E+W+S+A+E+ C11       #= O+ 10*C21+ 100*C22,
   E+A+W+O+O+A+O+A+O+L+O+L+O+E+O+E+ C21+ C12  #= O+ 10*C31+ 100*C32,
   H+C+W+H+S+E+C+B+H+A+H+P+C+E+S+H+ C31+ C22  #= W+ 10*C41+ 100*C42,
   C+  O+C+L+L+     P+  H+H+S+ C41+ C32  #= L+ 10*C51+ 100*C52,
   B+  A+A+     A+  C+C      + C51+ C42  #= L+ 10*C61,
   P               + C61+ C52  #= A,


   flatten([LD,CarriesLow,CarriesHigh], Vars),
   labeling([ff,enum],Vars).

