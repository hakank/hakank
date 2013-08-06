/*

  Set partition problem in ECLiPSe.

  Problem formulation from
    http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
   This is a partition problem.
   Given the set S = {1, 2, ..., n}, 
   it consists in finding two sets A and B such that:
   <ul>
   <li>A U B = S,</li>
   <li>|A| = |B|,</li>
   <li>sum(A) = sum(B),</li>
   <li>sum_squares(A) = sum_squares(B).</li>
   </ul>
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_partition.mzn
  * Gecode/R: http://www.hakank.org/gecode_r/set_partition.rb
  * Comet   : http://www.hakank.org/comet/set_partition.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_sets).
:-lib(ic_search).
:-lib(listut).



% find all (7) solutions for N = 16.
go :-
        N = 16,
        NumSets = 2,
        set_partition(N, NumSets), nl,fail.

%
% check for a solution between N = 17 and 32
%
go2 :-
        ic : '::'(N,17..32),
        indomain(N),
        NumSets = 2,
        writeln(n:N),
        set_partition(N, NumSets).


%
% Here we find the minimal N and NumSets for
% a solution to the problem.
%
go3 :-
        ic: '::'(N, 2..20),
        ic: '::'(NumSets, 3..9),
        indomain(N),
        indomain(NumSets),
        writeln([n:N,num_sets:NumSets]),
        set_partition(N,NumSets).

        

set_partition(N,NumSets) :-


        % sanity check
        Mod is N mod NumSets,
        ( Mod \= 0 -> 
              printf("Error: %d is not a multiple of %d\n", [NumSets,N]),
              fail
        ;
              true
        ),
              

        % list of sets
        intsets(A,NumSets,1,N),

        % sums
        dim(Sums,[NumSets]),
        N2 is N*N,
        ic: '::'(Sums,0..N2),

        % squared sums
        dim(SumSquared,[NumSets]),
        N4 is N2*N2,
        ic :'::'(SumSquared,0..N4),


        % create the universe for partition_set
        % and the weights for weight/3 below.
        dim(Weights,[N]),
        dim(Weights2,[N]),
        ( for(I,1,N), foreach(L,Universe), foreacharg(W, Weights),
          foreacharg(W2, Weights2) do
              L is I,
              W is I,
              W2 is I*I
        ),

        % same number of elements
        partition_set(A, Universe),

        % all sets must have the same cardinality
        same_cardinality(A),

        % % calculate sums and squared sums for each partition
        % ( for(I,1,NumSets), param(A,Sums,SumSquared,N) do
        %      listut:nth1(I,A,AI),
        %      ( for(J,1,N), 
        %        fromto(0,In1,In1+J*InSet,SumsTmp),
        %        fromto(0,In2,In2+J*J*InSet,SumSquaredTmp),
        %        param(AI) do
        %            InSet #= (J in AI)
        %      ),
        %      Sums[I] #= eval(SumsTmp),
        %      SumSquared[I] #= eval(SumSquaredTmp)
        % ),

        % Calculate sums and squared sums, using weight/3 instead
        % This is faster than the one above.
        ( for(I,1,NumSets), param(A,Weights,Weights2,Sums,SumSquared) do
              listut:nth1(I,A,AI),
              Sums[I] #= weight(AI,Weights),
              SumSquared[I] #= weight(AI,Weights2)
        ),
        
        % all sums and squared sums must be equal
        ( for(I,1,NumSets-1), param(Sums, SumSquared) do
              I1 is I+1,
              Sums[I] #= Sums[I1],
              SumSquared[I] #= SumSquared[I1]
        ),

        % symmetry breaking
        nth1(1, A, A1),
        1 in A1,


        %
        % search
        % 
        term_variables([Sums,SumSquared], Vars),

        % It is much faster if we first label the sets.
        label_sets(A),
        search(Vars,0,smallest,indomain_min,complete,
               [backtrack(Backtracks)]),

        writeln(a:A),
        writeln(sums:Sums),
        writeln(sum_squared:SumSquared),
        writeln(backtracks:Backtracks).



%
% labeling the sets
%
label_sets([]).
label_sets([S|Ss]) :-
	% insetdomain(S,_,_,_),
	insetdomain(S,increasing,big_first,in_notin),
	label_sets(Ss).


%
% Partitions the list of sets S into disjoint sets.
% All elements in the universe Universe must be
% included in exactly one of the sets.
%
partition_set(S, Universe) :-
        all_disjoint(S),
        all_union(S,Universe).


%
% all sets should have the same cardinality
% (walk through the list and compare two consequtive elements)
%

same_cardinality(S) :-
        ( fromto(S,[S1,S2|Rest],[S2|Rest], [_]) do
              #(S1) #= #(S2)
        ).

              

% another version...
same_cardinality2(S,NumSets) :-
	( for(I,1,NumSets-1), param(S) do
              listut:nth1(I,S,S1),
              I1 is I+1,
              listut:nth1(I1,S,S2),              
              #(S1) #= #(S2)              
	).

% yet another version
same_cardinality3([]) :- !.
same_cardinality3([_]) :- !.  
same_cardinality3([S1,S2|Rest]) :- #(S1) #= #(S2), same_cardinality([S2|Rest]).

