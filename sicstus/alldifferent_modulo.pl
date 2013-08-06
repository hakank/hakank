/*

  Global constraint alldifferent_modulo in SICStus Prolog.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_modulo.html
  """
  Enforce all variables of the collection VARIABLES to have a distinct 
  rest when divided by M.
  
  Example
  (<25, 1, 14, 3>, 5)
  
  The equivalence classes associated with values 25, 1, 14 and 3 are 
  respectively equal to 
     25 mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod = 3. 
  Since they are distinct the alldifferent_modulo constraint holds.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/alldifferent_modulo.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 4,
        length(X,N),
        domain(X,1,25),

        X = [25,1,14,3],

        M in 1..5,
        % indomain(M),
        M = 5,

        alldifferent_modulo(X,M),

        append(X,[M],Vars),
        labeling([], Vars),

        write(m:M),nl,
        write(x:X),nl,
        % fail,
        fd_statistics.
        

alldifferent_modulo(Xs,M) :-
        ( foreach(X,Xs),
          foreach(Y,Ys),
          param(M) do
              Y #= X mod M
        ),
        all_different(Ys).
