/*

  DONALD + ROBERT = GERALD in SICStus Prolog.

  Classic alphametic problem.

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/donald_gerald.co
  * ECLiPSe: http://www.hakank.org/eclipse/donald_gerald.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        X = [D,O,N,A,L,G,E,R,B,T],
        domain(X,0,9),

        all_different(X),

           100000*D + 10000*O + 1000*N + 100*A + 10*L + D 
         + 100000*G + 10000*E + 1000*R + 100*A + 10*L + D
        #= 100000*R + 10000*O + 1000*B + 100*E + 10*R + T,

        D #> 0,
        G #> 0,
        R #> 0,

        labeling([],X),

        write(X),nl.


