/*

  Global constraint among in ECLiPSe.

  Among: Requires exactly 'n' variables in 'x' to take one of the values in 'v'.

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong.html
  """
  Constraint
    among​(NVAR,​VARIABLES,​VALUES)
  ...
  Purpose
    NVAR is the number of variables of the collection VARIABLES that
    take their value in VALUES.

  Example:
  (3, <4, 5, 5, 4, 1>, <1,​5,​8>)

  The among constraint holds since exactly 3 values of the collection
  of values 
  <4, 5, 5, 4, 1> belong to the set of values {1, 5, 8}.
  """


  Compare with the following Comet model:
  * http://www.hakank.org/comet/among.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_sets).


go :-

        Len = 5,
        R = 1..8,

        %
        % The set {1,5,8} (as a list).
        %
        % Note: If there are duplicates in V (as a list)
        % then in(El, V) in among/3 below will fail.
        %
        V = [1,5,8],

        % create X
        length(X,Len),
        ic:(X :: R),

        % X = [4,5,5,4,1], % the example above

        % N: number of elements in X that is in S
        ic: (N :: 1..Len),
        N #= 3,

        among(N, X, V),

        % search
        term_variables([X,N], Vars),
        labeling(Vars),

        writeln(n:N),
        writeln(x:X),
        writeln(v:V),
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
        ( foreach(El,X), fromto(0,In,In + (El in V),Sum), param(V) do
              true
        ),
        N #= eval(Sum).
