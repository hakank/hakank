/*

  Global constraint all_differ_from_at_least_k_pos in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        Rows = 3,
        Cols = 4,

        matrix(X,[Rows,Cols]),
        term_variables(X,XList),
        XList :: 0..6,

        K in 0..Cols,
        K #= 2,

        % the example above
        % X = [[2,5,2,0],
        %      [3,6,2,1],
        %      [3,6,1,0]],

        X = [[2,5,2,_],
             [3,6,2,1],
             [3,6,1,0]],
        

        % fails
        % X = [[2,5,2,0],
        %     [2,5,2,1],
        %     [3,6,1,0]],

        all_differ_from_at_least_k_pos(K,X),

        term_variables([XList,K], Vars),
        labeling([],Vars),

        write(k:K),nl,
        foreach(Row in X, writeln(Row)),
        nl.



all_differ_from_at_least_k_pos(_,[]) :- !.
all_differ_from_at_least_k_pos(K,[Row|Rows]) :-
        foreach(Row2 in Rows,
                sum([(R1 #\= R2) : (R2,R1) in (Row,Row2)]) #>= K
        ),
        all_differ_from_at_least_k_pos(K,Rows).

matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        foreach(X in L, matrix(X, Dims)).



