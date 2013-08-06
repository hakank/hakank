/*

  (Decomposition of) global constraint nvlaues in ECLiPSe.

  Reference: 
  Clobal Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cnvalues.html
  """
  Purpose
 
      Let N be the number of distinct values assigned to the variables of the 
      VARIABLES collection. Enforce condition N <RELOP> LIMIT to hold.
 
  Example
      (<4,5,5,4,1,5>,=,3)
 
      The nvalues constraint holds since the number of distinct values occurring within 
      the collection 4,5,5,4,1,5 is equal (i.e., RELOP is set to =) to its 
      third argument LIMIT=3.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/nvalues.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


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


%
% nvalues(X,Op,N)
%
% Requires that the number of distinct values in the array X is 
%    Op N 
% where
% Op is either one of 
%   #=, #<m, #=<, #>=, #>
% (this is not checked though)    
%
nvalues(X, Op, N) :-
        nvalue(M,X),
        T =.. [Op,M,N], 
        T.

go :-
        Len = 5,
        dim(X,[Len]),
        X :: 1..Len,
        N = 2, 

        nvalues(X,#=<,N),
        nvalue(N2,X), % So may many different values was it?

        term_variables([X],Vars),
        search(Vars,0,first_fail,indomain,complete,[]),

        writeln([n:N,n2:N2,x:X]),
        fail.
