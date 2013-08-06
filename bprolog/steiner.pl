/*

  Steiner triplets in B-Prolog.

  http://www.probp.com/examples/clpset/steiner.pl
  """
  The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements 
  in {1,2,...,n} such that each set contains three elements and any two 
  sets have at most one element in common.

  For example, the following shows a solution for size n=7:

      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

  Problem taken from:
  C. Gervet: Interval Propagation to Reason about Sets: Definition and 
             Implementation of a PracticalLanguage,  
             Constraints, An International Journal, vol.1, pp.191-246, 1997.
  """


  Note: This model uses arrays of booleans as an representation of sets.
  Compare with the following model with the same principle:
  * Comet: http://www.hakank.org/comet/steiner_triplet.co
  * SICStus Prolog: http://www.hakank.org/sicstus/steiner.co


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 9,
        steiner(N,Steiner),
        write(Steiner),nl.


steiner(N,Steiner) :-

        % number of sets
        Nb is (N * (N-1)) // 6,

        matrix(Sets,[Nb,N]),
        term_variables(Sets,SetsList),
        SetsList :: 0..1,

        % atmost 1 element in common
        foreach((S1,I) in (Sets,1..Nb),
                ( 
                    3 #= sum(S1), % cardinality
                    foreach((S2,J) in (Sets,1..Nb),
                            [Common],
                            (
                                I > J -> 
                                    union_card(S1,S2,Common),
                                    Common #=< 1
                            ;
                                    true
                            )
                           )
                )
        ),

        labeling([constr,down],SetsList),
        
        % convert to set representation
        Steiner @= [Res : SS in Sets, [Res], boolean_to_set(SS,Res)].

        

%
% number of common elements in two "sets"
%
union_card(S1,S2,CardCommon) :-
        CardCommon #= sum([(SS1 + SS2 #= 2) : (SS1,SS2) in (S1,S2)]).

%
% convert a list of boolean to a "set"
%
boolean_to_set(List,Set) :-
        length(List,Len),
        Set @= [I : (C,I) in (List, 1..Len), C == 1].


matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        foreach(X in L, matrix(X, Dims)).
