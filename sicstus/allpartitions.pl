/*

  All partitions in SICStus Prolog.

  Simple implementation of all partitions.

  For the number of different partitions, see
  The On-Line Encyclopedia of Integer Sequences:
  http://www.research.att.com/~njas/sequences/A000041


  Compare with the following model:
  * ECLiPSe: http://www.hakank.org/eclipse/allpartitions.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% Y is list L with value Val removed
%
remove_val(L,Val,Y) :-
        ( foreach(El, L),
          fromto(Y, Out, In, []),
          param(Val) 
        do
              El #\= Val
        ->
          Out = [El|In]
        ;    
          Out = In
        ).

%
% The first part is simply to get an ordered array where the sum is N.
% This ordering is needed for removing symmetries when all
% 0's is remove in the second part.
% 
allpartitions(N, Xs) :-

        % part I: get all candidates
        length(X, N),
        domain(X, 0,N),
        sum(X,#=,N),
        my_ordered(X),
       
        labeling([ff,bisect,up], X),
        
        % part II: now remove all 0's from X
        remove_val(X, 0, Xs).

my_ordered(List) :-
        ( List = [] -> 
              true
        ;
              ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
              do
                This #=< Next
              )
        ).

go :-
        N = 34,
        findall(X, allpartitions(N, X),L),
        length(L, Len),

        % for larger N we really don't want to print all partitions
        ( N =< 8 ->
              ( foreach(El, L) do write(El),nl )
        ; 
              true
        ),
        write([len:Len]),nl,
        fd_statistics.
