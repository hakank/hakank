/*

  Rabbits problem in SWI Prolog

  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 9.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   N = 20,
   NbRabbits in 0..N,
   NbPheasants in 0..N,
   
   20 #= NbRabbits + NbPheasants,
   56 #= 4*NbRabbits + 2*NbPheasants,

   labeling([],[NbRabbits,NbPheasants]),

   writeln(nbRabbits=NbRabbits),
   writeln(nbPheasants=NbPheasants),
   nl.

