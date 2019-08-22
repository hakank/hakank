/*

  DONALD + GERALD = ROBERT in SWI Prolog

  Classic alphametic problem.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        X = [D,O,N,A,L,G,E,R,B,T],
        X ins 0..9,

        all_different(X),

           100000*D + 10000*O + 1000*N + 100*A + 10*L + D 
         + 100000*G + 10000*E + 1000*R + 100*A + 10*L + D
        #= 100000*R + 10000*O + 1000*B + 100*E + 10*R + T,

        D #> 0,
        G #> 0,
        R #> 0,

        labeling([ffc],X),

        writeln(X).


