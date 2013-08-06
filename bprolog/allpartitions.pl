/*

  All partitions in B-Prolog.

  Simple implementation of all partitions.

  For the number of different partitions, see
  The On-Line Encyclopedia of Integer Sequences:
  http://www.research.att.com/~njas/sequences/A000041


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        N :: 2..34,
        indomain(N),

        findall(X, allpartitions(N, X),L),
        length(L, Len),

        % for larger N we really don't want to print all partitions
        ( N =< 8 ->
              foreach(El in L, writeln(El))
        ; 
              true
        ),
        writeln([n:N,len:Len]),
        fail.


%
% Y is list L with value Val removed
%
remove_val(L,Val,Y) :-
        Y @= [El : El in L, El #\= Val].

%
% The first part is simply to get an ordered array where the sum is N.
% This ordering is needed for removing symmetries when all
% 0's is remove in the second part.
% 
allpartitions(N, Xs) :-

        % part I: get all candidates
        length(X, N),
        X :: 0..N,
        N #= sum(X),
        increasing(X),
       
        labeling([ffc,down], X),

        % part II: now remove all 0's from X
        remove_val(X, 0, Xs).

increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).
