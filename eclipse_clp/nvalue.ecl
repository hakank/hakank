/*

  (Decomposition of) global constraint nvalue in ECLiPSe.

  From MiniZinc:
  """
  Requires that the number of distinct values in 'x' is 'n'.
  """

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_search).

%
% nvalue(?N,?X)
%
% Requires that the number of distinct values in X is N.
%
nvalue(N, X) :-
        dim(X,[Len]),
        ( for(I,1,Len), fromto(0,In1,Out1,Sum1), param(X,Len) do
              ( for(J,1,Len), fromto(0,In2,Out2,Sum2), param(X,I) do
                    Out2 = In2 + (X[J] #= I)
              ),
              Out1 = In1 + eval(Sum2 #> 0)
        ),
        N #= eval(Sum1).

go :-
        Len = 5,
        dim(X,[Len]),
        X :: 1..Len,
        N :: 1..Len,        

        nvalue(N,X),

        term_variables([X],Vars),
        search(Vars,0,first_fail,indomain,complete,[]),

        writeln([n:N, x:X]),
        fail.
        
        