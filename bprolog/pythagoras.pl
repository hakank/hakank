/*

  Pythagoras problem in B-Prolog.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go:- 
        findall([A^2+B^2=C^2],pythagoras([A,B,C], 100),L), 
        write_list(L).


write_list(L) :-
        foreach(El in L, writeln(El)),
        nl.


pythagoras(LD, Max):-
     LD = [A,B,C],
     LD :: 1..Max,
     A**2#=A2,
     B**2#=B2,
     C**2#=C2,
     A2+B2 #= C2,
     A #=< B,
     B #=< C,
     labeling([],LD).

