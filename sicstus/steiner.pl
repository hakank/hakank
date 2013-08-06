/*

  Steiner triplets in SICStus Prolog.

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

  Also see:
  - http://mathworld.wolfram.com/SteinerTripleSystem.html
  - http://en.wikipedia.org/wiki/Steiner_system


  Note: This model uses arrays of booleans as an representation of sets.
  Compare with the following model with the same principle:
  * Comet: http://www.hakank.org/comet/steiner_triplet.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 9,
        steiner(N,Steiner),
        write(Steiner),nl,
        fd_statistics.


steiner(N,Steiner) :-

        % number of sets
        Nb is (N * (N-1)) // 6,

        matrix(Sets,[Nb,N]),
        append(Sets,SetsList),
        domain(SetsList,0,1),

        % cardinality is 3
        % ( foreach(S,Sets) do
        %      sum(S,#=,3)
        % ),
        
        % atmost 1 element in common
        ( foreach(S1,Sets), count(I,1,_),
          param(Sets) do
              sum(S1,#=,3), % cardinality
              ( foreach(S2,Sets), count(J,1,_),
                param(I,S1) do
                    I > J -> 
                    union_card(S1,S2,Common),
                    Common #=< 1
              ;
                    true
              )
        ),

        % lex_chain(Sets,[op(#=<)]),

        labeling([ffc,bisect,down],SetsList),
        
        % convert to set representation
        ( foreach(SS,Sets),
          fromto(Steiner,Out,In,[]) do
              boolean_to_set(SS,Res),
              Out = [Res|In]              
        ).

        

%
% number of common elements in two "sets"
%
union_card(S1,S2,CardCommon) :-
        ( foreach(SS1,S1),
          foreach(SS2,S2),
          fromto(Sum,Out,In,[]) do
              SSSum #= SS1 + SS2,
              Reif in 0..1,
              SSSum #= 2 #<=> Reif #= 1,
              Out = [Reif|In]
        ),
        sum(Sum,#=,CardCommon).

%
% convert a list of boolean to a "set"
%
boolean_to_set(List,Set) :-
        ( foreach(C,List),
          count(I,1,_),
          fromto(Set,Out,In,[]) do
              C == 1 
        ->
          Out = [I|In]
        ;
          Out = In
        ).


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% Suggested by Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
