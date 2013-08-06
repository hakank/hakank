/*

  Global constraint alldifferent_on_intersection in ECLiPSe.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_on_intersection.html
  """
  The values that both occur in the VARIABLES1 and VARIABLES2 collections 
  have only one occurrence.
  
  Example
  (
   <5, 9, 1, 5>,
   <2, 1, 6, 9, 6, 2>
  )
  
  The alldifferent_on_intersection constraint holds since the values 9 and 1 
  that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
  exactly one occurrence in each collection.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/alldifferent_on_intersection.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/alldifferent_on_intersection.pl

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
        M = 4,
        N = 6,

        length(X,M),
        X :: 1..9,

        length(Y,N),
        Y :: 1..9,

        % X = [5,9,1,5],
        X = [5,9,_,5],

        Y = [2,1,6,9,6,2], % constraint holds
        % Y = [2,1,6,9,6,1], % constraint do not hold since 
                            %  there are two 1's in 1 and one 1 in x

        alldifferent_on_intersection(X,Y),

        term_variables([X,Y],Vars),
        labeling(Vars),

        writeln(x:X),
        writeln(y:Y),
        nl,
        fail.


alldifferent_on_intersection(Xs,Ys) :-
        count_A_in_B(Xs,Ys),
        count_A_in_B(Ys,Xs).


count_A_in_B(As,Bs) :-
        ( foreach(A,As),
          param(Bs) do
              ( foreach(B,Bs),
                fromto(0,In,Out,Sum),
                param(A) do
                    Reif #= (A #= B),
                    Out #= In + Reif

              ),
              Sum #=< 1 
        ).