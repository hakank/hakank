/*

  Set partition problem in B-Prolog.

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

  This model just implementa a two set version.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


% find all (7) solutions for N = 16.
go :-
        findall(Sets,set_partition(16, Sets),L), 
        length(L,Len),
        format("It was ~d solutions.\n", [Len]).


go2 :-
        N :: 2..50,
        indomain(N),
        once(set_partition(N, _Sets)), 
        nl,fail.



set_partition(N,Sets) :-

        writeln('\nN':N),

        (N mod 2 > 0 -> format("~d is not a multiple of 2.\n", [N]), fail ; true ),
        (N mod 4 > 0 -> format("~d is not a multiple of 4.\n", [N]), fail ; true ),

        N2 is N //2,

        [A,B] :: {}..{1..N},
        Sets = [A,B],

        % same cardinality
        #A #= N2,
        #A #= #B,

        % A and B are disjoint
        A #<> B,

        % Symmetry breaking: 1 is in the A set
        1 #<- A,

        % All numbers 1..N must be in some set
        foreach(I in 1..N, (I #<- A ; I #<- B)),

        % It seems that we must have the labeling
        % already here...
        indomain(A),
        indomain(B),

        set_to_list(A,AList),
        set_to_list(B,BList),


        % sum of the sets must be equal
        ASum #= sum(AList),
        BSum #= sum(BList),
        ASum #= BSum,

        % sum of the squares must be equal
        ASumSquared #= sum([ T**2 : I in 1..AList^length, [T], T @= AList[I]]),
        BSumSquared #= sum([ T**2 : I in 1..BList^length, [T], T @= BList[I]]),

        ASumSquared #= BSumSquared,

        % labeling([ASum,BSum,ASumSquared,BSumSquared]),

        writeln(A),
        writeln(B),
        writeln(sums:[ASum,BSum]),
        writeln(sumSquares:[ASumSquared,BSumSquared]),
        nl.
