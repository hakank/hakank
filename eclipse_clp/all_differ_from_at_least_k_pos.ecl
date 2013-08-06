/*

  Global constraint all_differ_from_at_least_k_pos in ECLiPSe.


  From Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Call_different_from_at_least_k_pos.html
  """
  Enforce all pairs of distinct vectors of the VECTORS collection to differ 
  from at least K positions.
  
  Example
  (
   2, <
   vec-<2, 5, 2, 0>,
   vec-<3, 6, 2, 1>,
   vec-<3, 6, 1, 0>
   >
 )
  
  The all_differ_from_at_least_k_pos constraint holds since:
   * The first and second vectors differ from 3 positions, which is 
     greater than or equal to K=2.
   * The first and third vectors differ from 3 positions, which is greater 
     than or equal to K=2.
   * The second and third vectors differ from 2 positions, which is greater 
     than or equal to K=2.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/all_differ_from_at_least_k_pos.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/all_differ_from_at_least_k_pos.pl


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
        Rows = 3,
        Cols = 4,

        matrix(X,[Rows,Cols]),
        term_variables(X,XList),
        XList :: 0..6,

        K :: 0..Cols,
        % K #= 2,

        % the example above
        X = [[2,5,2,0],
             [3,6,2,1],
             [3,6,1,0]],
        

        % fails
        % X = [[2,5,2,0],
        %     [2,5,2,1],
        %     [3,6,1,0]],

        all_differ_from_at_least_k_pos(K,X),

        term_variables([XList,K], Vars),
        labeling(Vars),

        writeln(k:K),
        ( foreach(Row,X) do
              write(Row),nl
        ),
        nl,fail.



% Note: If K is free, then all the K from
% 0.."the real K" will be generated, starting
% from 0.
all_differ_from_at_least_k_pos(_,[]) :- !.
all_differ_from_at_least_k_pos(K,[Row|Rows]) :-
        ( foreach(Row2,Rows),
          param(Row,K) do
              ( foreach(R1,Row),
                foreach(R2,Row2),
                fromto(0,In,Out,Diff) do
                    Reif :: 0..1,
                    Reif #= (R1 #\= R2),
                    Out #= In + Reif
              ),
              Diff #>= K
        ),
        all_differ_from_at_least_k_pos(K,Rows).



matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).

matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).

