/*

  Global constraint alldifferent_cst in ECLiPSe.
 
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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/alldifferent_cst.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_cst.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(listut).
%:-lib(propia).




go :- 
        N = 4,

        Cst = [1,2,1,4],
        % Cst = [0,1,0,4],
        % Cst = [0,0,0,0], % for plain all_different 
        length(X, N),
        X :: 1..9,

        % X = [5,1,9,3],
        % X = [5,_,9,3],
        alldifferent_cst(X, Cst),

        labeling(X),

        writeln(X),fail.
        

go2 :-
        N = 4,
        Cst = [1,2,1,4],
        % Cst = [0,1,0,4],
        % Cst = [0,0,0,0], % for plain all_different 
        length(X, N),
        X :: 1..9,

        % X = [5,1,9,3],
        X = [5,_,9,3],
        findall(X,( alldifferent_cst(X, Cst),labeling(X)), L),
        writeln(L).




alldifferent_cst(Xs, Cst) :-
        ( foreach(X,Xs),
          foreach(C,Cst),
          fromto(Res,Out,In,[]) do
              XC #= X + C,
              Out = [XC|In]
        ),
        alldifferent(Res).


