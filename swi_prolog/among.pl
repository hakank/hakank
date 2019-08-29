/*

  Global constraint among in SWI Prolog

  Among: Requires exactly 'n' variables in 'x' to take one of the values in 'v'.

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong.html
  """
  Constraint
    among(NVAR,VARIABLES,VALUES)
  ...
  Purpose
    NVAR is the number of variables of the collection VARIABLES that
    take their value in VALUES.

  Example:
  (3, <4, 5, 5, 4, 1>, <1,5,8>)

  The among constraint holds since exactly 3 values of the collection
  of values 
  <4, 5, 5, 4, 1> belong to the set of values {1, 5, 8}.
  """

  Note: among/3 is defined in http://hakank.org/swi_prolog/hakank_utils.pl

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(library(ordsets)).

go :-
        Len = 5,
        length(X,Len),
        X ins 1..8,

        V = [1,5,8],

        %% X = [4,5,5,4,1], % the example above
       
        %% N: number of elements in X that is in V
        N in 1..Len,
        N #= 3,

        among(N, X, V),

        label(X),
        writeln(v=V),
        writeln(x=X),
        writeln(n=N),
        nl.


go2 :-

        Len = 5,
        length(X,Len),
        X ins 1..8,

        V = [1,5,8],

        %% N: number of elements in X that is in V
        N in 1..Len,
        N #= 3,

        findall([x=X,n=N,v=V],
                (
                 among(N, X, V),
                 label(X)
                ),
                L),
        
        length(L,Len2),
        maplist(writeln,L),
        nl,
        writeln(len=Len2),
        nl.

%%
%% Now we let V and N be free as well,
%%
go3 :-
        Len = 5,
        length(X,Len),
        X ins 1..4,

        length(V,3),
        V ins 1..3,

        %% N: number of elements in X that is in V
        N in 1..Len,

        findall([x=X,n=N,v=V],
                (
                 among(N, X, V),
                 flatten([X,V,N],Vars),
                 label(Vars)
                ),
                L),
        
        length(L,Len2),
        maplist(writeln,L),
        nl,
        writeln(len=Len2),
        nl.
