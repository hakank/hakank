/*

  Magic sequence problem in B-Prolog.

  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


go :-
        time2(solve(400,_Sequence)).


go2 :-
        foreach(N in 4..40,
                [Sequence],
                time2(solve(N,Sequence))
               ).

solve(N, Sequence) :-
        format('\n~d: ',[N]),
        length(Sequence, N),
        Sequence :: 0..N-1,

        % Note: I would like to use global_cardinality/2 instead 
        %       but didn't get it right.
        foreach(I in 0..N-1, count(I,Sequence,#=,Sequence[I+1])),
        % Alternative
        % foreach(I in 0..N-1, exactly(Sequence[I+1],Sequence,I)),

        N #= sum(Sequence),
        Integers @= [ I : I in 0..N-1],
        scalar_product(Integers, Sequence, #=, N),

        % labeling([inout],Sequence) -> 
        labeling([degree],Sequence) -> 
        % labeling([ff],Sequence) -> 
            writeln(Sequence)
        ;
            writeln('Something wrong happened.'),
            true.
