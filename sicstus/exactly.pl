/*

  (Decomposition of) global constraint exactly in SICStus Prolog.

  From MiniZinc:
  """
  Requires exactly 'n' variables in 'x' to take the value 'v'.
  """

  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/exactly.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% exactly(?N,?X,?N)
%
% Requires exactly N variables in X to take the value V.
%
exactly(N, X, V) :-
        length(X,Len),
        ( for(I,1,Len), 
          fromto(0,In,Out, Sum), 
          param(X,V) do
              B in 0..1,
              element(I,X,XI),
              XI #= V #<=> B#=1,
              Out #= In+B
        ),
        N #= Sum.
        

go :-
        N in 2..3, % N: number of V in X
        V in 1..4, % V: the value

        Len = 4,
        length(X,Len),
        domain(X,1,Len),

        exactly(N,X,V),

        append(X,[N,V], Vars),
        labeling([],Vars),

        write([n:N,v:V]),nl,
        write(x:X),
        nl,nl,
        fail.

  