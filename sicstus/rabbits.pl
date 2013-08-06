/*

  Rabbits problem in SICStus Prolog.

  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 9.

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/rabbits.co
  * ECLiPSe: http://www.hakank.org/eclipse/rabbits.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 20,
        NbRabbits in 0..N,
        NbPheasants in 0..N,
        
        20 #= NbRabbits + NbPheasants,
        56 #= 4*NbRabbits + 2*NbPheasants,

        labeling([],[NbRabbits,NbPheasants]),

        write(nbRabbits:NbRabbits),nl,
        write(nbPheasants:NbPheasants),nl.
