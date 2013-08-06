/*

  Set partition problem in SICStus Prolog.

  Problem formulation from
  http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n}, 
   it consists in finding two sets A and B such that:
    - A U B = S,</li>
    - |A| = |B|,</li>
    - sum(A) = sum(B),</li>
    - sum_squares(A) = sum_squares(B).
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_partition.mzn
  * Gecode/R: http://www.hakank.org/gecode_r/set_partition.rb
  * Comet   : http://www.hakank.org/comet/set_partition.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_partition.ecl


  Note: This model uses boolean lists instead of set vars.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


% find all (7) solutions for N = 16.
go :-
        N = 16,
        NumSets = 2,
        findall([A,Sums,SumSquared],set_partition(N, NumSets,A,Sums,
                                                  SumSquared),L),
        length(L,Len),
        write(len:Len),nl,
        ( foreach([A,Sums,SumSquared],L) do
              write(a:A),nl,
              write(sums:Sums),nl,
              write(sum_squared:SumSquared),nl,nl
        ),

        fd_statistics.


%
% Search for a solution between N = 17 and 32
%
go2 :-
         N in 17..32,
         indomain(N),
         NumSets = 2,
         write(n:N),nl,
         set_partition(N, NumSets,A,Sums,SumSquared),
         write(a:A),nl,
         write(sums:Sums),nl,
         write(sum_squared:SumSquared),nl,
         fd_statistics.

%
% Test for larger N and more sets
%
go3 :-
        N in 4..127, % overflow for 127
        NumSets in 3..5,
        indomain(N),
        indomain(NumSets),
        write([n:N,num_sets:NumSets]),nl,
        set_partition(N, NumSets,A,Sums,SumSquared),
        write(a:A),nl,
        write(sums:Sums),nl,
        write(sum_squared:SumSquared),nl,
        fd_statistics.

       

%
% set partition
% 
set_partition(N,NumSets,ASet,Sums,SumSquared) :-

        % sanity check
        Mod is N mod NumSets,
        ( Mod \= 0 -> 
              format('Error: ~d is not a multiple of ~d\n', [NumSets,N]),
              fail
        ;
              true
        ),
              

        % list of sets
        % this corresponds to a list of NumSets sets of 1..N
        matrix(A,[NumSets,N]),
        append(A,AList),
        domain(AList,0,1),

        % sums
        length(Sums,NumSets),
        N2 is N*N,
        domain(Sums,0,N2),

        % squared sums
        length(SumSquared,NumSets),
        N4 is N2*N2,
        domain(SumSquared,0,N4),


        % create the universe for partition_set
        % and the weights for weight/3 below.
        length(Weights,N),
        length(Weights2,N),
        ( for(I,1,N), 
          foreach(L,Universe), 
          foreach(W, Weights),
          foreach(W2, Weights2) do
              L is I,
              W is I,
              W2 is I*I
        ),

        % same number of elements
        partition_set(A, Universe),

        % all sets must have the same cardinality
        same_cardinality(A),

        % calculate sums and squared sums for each partition
        ( for(I,1,NumSets), 
          foreach(SumsI,Sums),
          foreach(SumSquaredI,SumSquared),
          foreach(AI,A),
          param(A,Weights,Weights2) do
              scalar_product(Weights,AI,#=,SumsI),
              scalar_product(Weights2,AI,#=,SumSquaredI)
        ),
        
        % all sums and squared sums must be equal
        same_values(Sums),
        same_values(SumSquared),

        % symmetry breaking: 1 should be in the first set
        nth1(1, A, A1),
        contains(1,A1),

        % search
        append(AList,Sums,Vars1),
        append(Vars1,SumSquared, Vars),
        labeling([], Vars),

        % convert to set representation
        ( foreach(ARow,A),
          foreach(SetRow,ASet)
        do 
          boolean_to_set(ARow,SetRow)
        ).



% Element El is in the set Set
contains(El, Set) :-
        element(El,Set,1).


% all elements have the same values
same_values(X) :-
        ( fromto(X, [This,Next | Rest], [Next|Rest],[_]) do
              This #= Next
        ).


%
% Partitions the list of sets S into disjoint sets.
% All elements in the universe Universe must be
% included in exactly one of the sets.
%
partition_set(S, _Universe) :-
        all_disjoint(S).%,
        % all_union(S,Universe).


% Checks that either of the sets contains the value
%
% Note: This handles both all_disjoint and all_union
%       in the partition set constraint above.
%
all_disjoint(Sets) :-
        ( foreach(S1,Sets),
          count(I,1,_),
          param(Sets) do
              ( foreach(S2,Sets),
                count(J,1,_),
                param(S1,I) do
                    I < J ->
                    ( foreach(SS1,S1),
                      foreach(SS2,S2) do
                          SS1 + SS2 #= 1
                    )
              ;
                    true
              )
        ).


% all sets have the same cardinality
same_cardinality(S) :-
        ( fromto(S,[S1,S2|Rest],[S2|Rest], [_]) do
              cardinality(S1,Card),
              cardinality(S2,Card)
        ).

              
% another version
same_cardinality3([]) :- !.
same_cardinality3([_]) :- !.  
same_cardinality3([S1,S2|Rest]) :- 
        cardinality(S1,Card),
        cardinality(S2,Card),
        same_cardinality([S2|Rest]).


% the cardinality is simply the sum of a "set"
cardinality(Set,Card) :-
        sum(Set,#=,Card).


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


% convert a list of boolean to a "set"
% (used after labeling)
boolean_to_set(List,Set) :-
        ( foreach(El,List),
          count(I,1,_),
          fromto(Set,Out,In,[]) do
              El == 1 
        ->
          Out = [I|In]
        ;
          Out = In
        ).