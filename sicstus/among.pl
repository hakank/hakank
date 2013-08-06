/*

  Decomposition of global constraint among in SICStus Prolog.


  Among: Requires exactly 'n' variables in 'x' to take one of the values in 'v'.

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong.html
  """
  Constraint
    among‹(NVAR,‹VARIABLES,‹VALUES)
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


  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/among.co
  * ECLiPSe: http://www.hakank.org/eclipse/among.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-

        Len = 5,
        R = 8,

        % create X
        length(X,Len),
        domain(X,1, R),

        %
        % The set {1,5,8}        %
        V = {1,5,8},

        % X = [4,5,5,4,1], % the example above

        % N: number of elements in X that is in V
        N in 1..Len,
        N #= 3,

        among(N, X, V),

        % search
        append(X,[N], Vars),
        labeling([],Vars),

        write(n:N),nl,
        write(x:X),nl,
        write(v:V),nl,
        nl,
        fail.



/*
  among(n, x, v)

  From MiniZinc globals.mzn:
  """
  Requires exactly 'n' variables in 'x' to take one of the values in 'v'.
  """
*/
among(N,X,V) :-  
        ( foreach(El,X), 
          fromto(0,In,Out,Sum), 
          param(V) do
              B in 0..1,
              El in V #<=> B #= 1,
              Out #= In + B
        ),
        N #= Sum.
