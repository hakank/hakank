/*

  Least diff problem in SWI Prolog

  The model solves the following problem:
  
  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  Minimize the difference 
    ABCDE - FGHIJ


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        time(once(least_diff(L, Diff))),
        writeln(L),
        writeln(Diff),
        
        % Alternative approach
        time(once(least_diff2(L2, Diff2))),
        writeln(L2),
        writeln(Diff2),
        nl.

%
% "Standard" alphametic approach.
% 
least_diff(L,Diff) :-
        L = [A,B,C,D,E,  F,G,H,I,J],
        L ins 0..9,
        
        all_distinct(L),
        
        X #= 10000*A + 1000*B + 100*C + 10*D + E,
        Y #= 10000*F + 1000*G + 100*H + 10*I + J,
        
        Diff #= X - Y,
        Diff #> 0,
        labeling([enum, min(Diff)], L).


%
% Alternative version using scalar_product/4.
%
least_diff2(L,Diff) :-
        L = [A,B,C,D,E,  F,G,H,I,J],
        L ins 0..9,
        all_distinct(L),
        length(L, Len),
        M #= Len div 2,

        findall(T, (between(1,M,I), MI #= M-I, T #= 10^MI), Base),
        scalar_product(Base,[A,B,C,D,E], #=, X),
        scalar_product(Base,[F,G,H,I,J], #=, Y),
        
        Diff #= X - Y,
        Diff #> 0,
        labeling([min(Diff)], L).

