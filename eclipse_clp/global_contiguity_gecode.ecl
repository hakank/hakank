/*

  Global contiguity in ECLiPSe.

  Using Gecode's regular.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(gfd).

go :-
        length(Xs,14),
        Xs :: 0..1,

        global_contiguity(Xs),

        labeling(Xs),
        writeln(Xs),
        fail.

global_contiguity(Xs) :-
        regular(Xs, (*(0) + *(1) + *(0))).
