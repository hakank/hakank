/*

  Global constraint alldifferent_modulo in ECLiPSe.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/alldifferent_modulo.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_modulo.pl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
:-lib(propia).


go :-
        N = 4,
        length(X,N),
        X :: 1..25,

        % X = [25,1,14,3],
        X = [25,_,14,3],

        M :: 1..5,
        % indomain(M),
        % M = 5,

        alldifferent_modulo(X,M),

        term_variables([X,M],Vars),
        labeling(Vars),

        writeln(m:M),
        writeln(x:X),
        fail.

        
% ECLiPSe's ic don't support the modulo operator
% so we use propia
alldifferent_modulo(Xs,M) :-
        ( foreach(X,Xs),
          foreach(Y,Ys),
          param(M) do
              (Y #= X mod M infers most)
        ),
        alldifferent(Ys).
