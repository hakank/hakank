/*

  Decomposition of global constraint among in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-

        Len = 5,
        R = 8,

        % create X
        length(X,Len),
        X :: 1..R,

        %
        % The set {1,5,8}
        % Note that this is a list
        V = [1,5,8],

        % X = [4,5,5,4,1], % the example above

        % N: number of elements in X that is in V
        N in 1..Len,
        N #= 3,

        findall([X,N],
                (
                    among(N, X, V),
                    term_variables([X,N], Vars),
                    labeling([],Vars)
                ),
                L
               ),
        length(L,Len2),
        foreach([X,N] in L,
                (
                    writeln(n:N),
                    writeln(x:X),
                    writeln(v:V),
                    nl
                )),
        nl,
        writeln(len:Len2),
        nl
        .



/*
  among(n, x, v)

  From MiniZinc globals.mzn:
  """
  Requires exactly 'n' variables in 'x' to take one of the values in 'v'.
  """
*/
among(N,X,V) :-  
        N #= sum([B :  El in X,[B], B #<=> El in V]).
