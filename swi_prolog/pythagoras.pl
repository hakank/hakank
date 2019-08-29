/*

  Pythagoras problem in SWI Prolog



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go:- 
        findall([A^2+B^2=C^2],pythagoras([A,B,C], 100),L), 
        maplist(writeln,L),
        nl.

pythagoras(LD, Max):-
        
     LD = [A,B,C],
     LD ins 1..Max,
     
     A^2 + B^2 #= C^2,
     
     A #=< B,
     B #=< C,
     
     labeling([ffc,bisect,down],LD).
