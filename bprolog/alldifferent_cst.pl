/*

  Global constraint alldifferent_cst in B-Prolog.

  From Global Constraint Catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_cst.html
  """
  For all pairs of items (VARIABLES[i], VARIABLES[j]) (i!=j) of the 
  collection VARIABLES enforce 
  VARIABLES[i].var+VARIABLES[i].cst != VARIABLES[j].var+VARIABLES[j].cst.
 
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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :- 
        N = 4,

        Cst = [0,1,0,4],
        % Cst = [0,0,0,0], % for plain all_different 
        length(X, N),
        X :: 1..9,

        % X = [5,1,9,3],
        findall([X,Cst], 
                (alldifferent_cst(X, Cst),
                 labeling(X)),L),
        length(L,Len),
        foreach([X,Cst] in L, writeln(X)),
        write(len:Len),nl.

        

alldifferent_cst(Xs, Cst) :-
        Res @= [(X + C) :  (X,C) in (Xs,Cst)],
        alldifferent(Res).


