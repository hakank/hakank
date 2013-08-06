/*

  Global constraint inverse in SICStus Prolog.

  Decomposition of global constraint inverse.

  From MiniZinc globals.mzn
  """
  Constrains two arrays of int variables, 'f' and 'invf', to represent
  inverse functions.  All the values in each array must be within the index
  set of the other array.
  """

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/inverse.co
  * ECLiPSe: http://www.hakank.org/eclipse/inverse.ecl


 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        % tests all solutions for N = 1..4
        N in 1..4,
        indomain(N),
        test(N), fail.


test(N) :-

        length(X, N), % as list
        domain(X,1,N),
        length(Y,N), % as list
        domain(Y,1,N),
        inverse_list(X,Y),

        append(X,Y, Vars),
        labeling([], Vars),

        write(x:X),nl,
        write(y:Y),nl,
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
        ( for(I,1,FLen), 
          param(F,InvF,InvFLen) do
              nth1(I,F,FI),
              ( for(J, 1,InvFLen), 
                param(InvF,I,FI) do
                    nth1(J,InvF,InvFJ),
                    J_FI in 0..1,
                    J #= FI #<=> J_FI #= 1,
                    I_InvFJ in 0..1,
                    I #= InvFJ #<=> I_InvFJ #= 1,
                    I_InvFJ #<=> J_FI
              )
        ).

