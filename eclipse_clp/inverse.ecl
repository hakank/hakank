/*

  Global constraint inverse in ECLiPSe.

  From MiniZinc globals.mzn
  """
  Constrains two arrays of int variables, 'f' and 'invf', to represent
  inverse functions.  All the values in each array must be within the index
  set of the other array.
  """

  Compare with the Comet model:
  http://www.hakank.org/comet/inverse.co

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_search).
:-lib(listut).


go :-
        % tests all solutions for N = 1..4
        ic:(N :: 1..4),
        indomain(N),
        test(N), fail.


test(N) :-

        % length(X, N), % as list
        dim(X, [N]),   % as array
        ic:(X :: 1..N),
        % length(Y,N), % as list
        dim(Y,[N]),   % as array
        ic:(Y :: 1..N),
        % inverse_list(X,Y),
        inverse_array(X,Y),

        term_variables([X,Y], Vars),
        search(Vars,0,first_fail,indomain_max, complete, []),

        writeln(x:X),
        writeln(y:Y),
        nl.



% From MiniZinc globals.mzn
%   """
% Constrains two arrays of int variables, 'f' and 'invf', to represent
% inverse functions.  All the values in each array must be within the index
% set of the other array.
% """
inverse_list(F,InvF) :-
        length(F, FLen),
        length(InvF, InvFLen),
        ( for(I,1,FLen), param(F,InvF,InvFLen) do
              nth1(I,F,FI),
              ( for(J, 1,InvFLen), param(InvF,I,FI) do
                    nth1(J,InvF,InvFJ),
                    (J #= FI) => (I #= InvFJ),
                    (I #= InvFJ) => (J #= FI)
              )
        ).

%
% using arrays
%
inverse_array(F,InvF) :-
        dim(F, [FLen]),
        dim(InvF, [InvFLen]),
        ( for(I,1,FLen), param(F,InvF,InvFLen) do
              FI #= F[I],
              ( for(J, 1,InvFLen), param(InvF,I,FI) do
                    InvFJ #= InvF[J], 
                    (J #= FI) => (I #= InvFJ),
                    (I #= InvFJ) => (J #= FI)
              )
        ).
