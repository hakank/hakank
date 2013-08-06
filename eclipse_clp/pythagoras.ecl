/*

  Pythagoras problem in ECLiPSe.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/


:- lib(ic).

go:- 
        findall([A^2+B^2=C^2],pythagoras([A,B,C], 100),L), 
        write_list(L).


write_list(L) :-
        ( foreach(El, L) do writeln(El)).


pythagoras(LD, Max):-
     LD = [A,B,C],
     LD :: 1..Max,
     A^2+B^2 #= C^2,
     A #=< B,
     B #=< C,
     labeling(LD).

