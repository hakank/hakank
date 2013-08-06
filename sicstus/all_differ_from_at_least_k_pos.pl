/*

  Global constraint all_differ_from_at_least_k_pos in SICStus Prolog.

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

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/all_differ_from_at_least_k_pos.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        Rows = 3,
        Cols = 4,

        matrix(X,[Rows,Cols]),
        append(X,XList),
        domain(XList,0,6),

        K in 0..Cols,
        K #= 2,

        % the example above
        X = [[2,5,2,0],
             [3,6,2,1],
             [3,6,1,0]],
        

        % fails
        % X = [[2,5,2,0],
        %     [2,5,2,1],
        %     [3,6,1,0]],

        all_differ_from_at_least_k_pos(K,X),

        append(XList,[K], Vars),
        labeling([],Vars),

        write(k:K),nl,
        ( foreach(Row,X) do
              write(Row),nl
        ),
        nl,

        fd_statistics.



all_differ_from_at_least_k_pos(_,[]) :- !.
all_differ_from_at_least_k_pos(K,[Row|Rows]) :-
        ( foreach(Row2,Rows),
          param(Row,K) do
              ( foreach(R2,Row),
                foreach(R1,Row2),
                fromto(0,In,Out,Diff) do
                    Reif in 0..1,
                    (R1 #\= R2) #<=> (Reif #= 1),
                    Out #= In + Reif
              ),
              Diff #>= K
        ),
        all_differ_from_at_least_k_pos(K,Rows).



matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).

