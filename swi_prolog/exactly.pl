/*

  (Decomposition of) global constraint exactly in SWI Prolog

  From MiniZinc:
  """
  Requires exactly 'n' variables in 'x' to take the value 'v'.
  """

  Note:
  exactly/3 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        %% N: number of V in X
        N in 2..3,
        %% V: the value
        V in 1..4,

        Len = 4,
        length(X,Len),
        X ins 1..Len,

        exactly(N,X,V),

        flatten([X,N,V], Vars),
        labeling([],Vars),

        writeln([n=N,v=V]),
        writeln(x=X),
        nl,
        fail.

go.

 

