/*

  Global constraint alldifferent_cst in SICStus Prolog.
 
  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
  """
  For all pairs of items‹(VARIABLES[i], VARIABLES[j]) (i!=j) of the 
  collection VARIABLES enforce 
  VARIABLES‹[i].var+VARIABLES‹[i].cst != VARIABLES‹[j].var+VARIABLES[j].cst.
 
  
  Example
   (<
      var-5 cst-0,
      var-1 cst-1,
      var-9 cst-0,
      var-3 cst-4
   >
   )
  
  The alldifferent_cst constraint holds since all the expressions 
  5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/alldifferent_cst.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :- 
        N = 4,

        Cst = [1,2,1,4],
        % Cst = [0,0,0,0], % for plain all_different 
        length(X, N),
        domain(X, 1,9),

        % X = [5,1,9,3],
        alldifferent_cst(X, Cst),

        labeling([], X),

        write(X),nl,
        fd_statistics.

        

alldifferent_cst(Xs, Cst) :-
        ( foreach(X,Xs),
          foreach(C,Cst),
          fromto(Res,Out,In,[]) do
              XC #= X + C,
              Out = [XC|In]
        ),
        all_different(Res).


