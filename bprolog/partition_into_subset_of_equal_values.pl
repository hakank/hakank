/*

  Partition into subset of equal sums in B-Prolog.

  From Programmers Stack Exchange (C#)
  http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
  Partitioning set into subsets with respect to equality of sum among subsets
  """
  let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
  numbers such that sum of subset1 should be equal to sum of subset2 
  {3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
  way might be dividable by 2 without any remainder) and if we can, we should 
  write our algorithm two create s1 and s2 out of s.
  
  How to proceed with this approach? I read partition problem in wiki and even in some 
  articles but i am not able to get anything. Can someone help me to find the 
  right algorithm and its explanation in simple English?
  """

  In my comment I show some possible solutions in MiniZinc and Google or-tools/C#:
  http://programmers.stackexchange.com/a/153215/13955

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        
        S = [3, 1, 1, 2, 2, 1, 5, 2, 7],
        length(S, N),
        NumSubsets = 2,
        % NumSubsets = 3,

        % decision variables
        length(X, N),
        X :: 1..NumSubsets,

        foreach(P in 1..NumSubsets-1,
                (sum([ (S[I]*(X[I] #= P)) : I in 1..N]) #=
                 sum([ (S[I]*(X[I] #= P+1)) : I in 1..N])
                )),

        Sum #= sum([ (S[I]*(X[I] #= 1)) : I in 1..N]),

        % Symmetry breaking
        X[1] #= 1,

        labeling(X),

        writeln(s:S),
        writeln(sum:Sum),
        writeln(x:X),

        foreach(P in 1..NumSubsets, [Indices,Values],
                (Indices @= [I : I in 1..N, X[I] =:= P],
                 Values @= [S[I] : I in 1..N, X[I] =:= P],
                 format("Partition ~d: Indices: ~w  Values: ~w.\n", [P,Indices, Values])
                )),
        nl.
