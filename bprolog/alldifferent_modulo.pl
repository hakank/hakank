/*

  Global constraint alldifferent_modulo in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N = 4,
        length(X,N),
        X in 1..25,
        % X = [25,1,14,3],

        % M in 1..5,
        % indomain(M),
        M = 5,

        findall([X,M], 
                (alldifferent_modulo(X,M),
                 term_variables([X,M],Vars),
                 labeling([], Vars)
                ),
                L),
        length(L,Len),
        foreach([X,M] in L, writeln([m:M,x:X])),
        writeln(len:Len).

        

alldifferent_modulo(Xs,M) :-
        Ys @= [X mod M : X in Xs],
        alldifferent(Ys).
