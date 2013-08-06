/*

  (Decomposition of) global constraint exactly in ECLiPSe.

  From MiniZinc:
  """
  Requires exactly 'n' variables in 'x' to take the value 'v'.
  """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).

%
% exactly(?N,?X,?N)
%
% Requires exactly N variables in X to take the value V.
%
exactly(N, X, V) :-
        dim(X,[Len]),
        ( for(I,1,Len), 
          fromto(0,In,In+(X[I] #= V), Sum), 
          param(X,V) do
              true
        ),
        N #= eval(Sum).
        

go :-
        % N: number of V in X
        N :: 2..3,
        % V: the value
        V :: 1..4,

        Len = 4,
        dim(X,[Len]),
        X :: 1..Len,

        exactly(N,X,V),

        term_variables([X,N,V], Vars),
        labeling(Vars),

        writeln([n:N,v:V]),
        writeln(x:X),
        nl,
        fail.

  