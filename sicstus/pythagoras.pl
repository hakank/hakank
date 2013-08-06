/*

  Pythagoras problem in SICStus Prolog.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/pythagoras.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/pythagoras.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go:- 
        findall([A^2+B^2=C^2],pythagoras([A,B,C], 100),L), 
        write_list(L),
        fd_statistics.


write_list(L) :-
        ( foreach(El, L) do write(El),nl).


pythagoras(LD, Max):-
     LD = [A,B,C],
     domain(LD,1,Max),
     power(A,2,A2),
     power(B,2,B2),
     power(C,2,C2),
     A2+B2 #= C2,
     A #=< B,
     B #=< C,
     labeling([ffc,bisect,down],LD).

power(A,N,Result) :-
        ( for(I,1,N),
          fromto(1,In,Out,Res),
          param(A)
        do
          Out #= In * A
        ),
        Result #= Res.