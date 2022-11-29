/*

  All partitions in SWI Prolog

  Simple implementation of all partitions.
  Also, I tried some different search strategies.

  For the number of different partitions, see
  The On-Line Encyclopedia of Integer Sequences:
  https://oeis.org/A000041

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        N = 8,
        findall(X, allpartitions(N, X),L),
        writeln(L),
        nl.


/*
  [[2],[1,1]]
  [n:2,len:2]
  [[3],[1,2],[1,1,1]]
  [n:3,len:3]
  [[4],[1,3],[2,2],[1,1,2],[1,1,1,1]]
  [n:4,len:5]
  [[5],[1,4],[2,3],[1,1,3],[1,2,2],[1,1,1,2],[1,1,1,1,1]]
  [n:5,len:7]
  [[6],[1,5],[2,4],[3,3],[1,1,4],[1,2,3],[2,2,2],[1,1,1,3],[1,1,2,2],[1,1,1,1,2],[1,1,1,1,1,1]]
  [n:6,len:11]
  [[7],[1,6],[2,5],[3,4],[1,1,5],[1,2,4],[1,3,3],[2,2,3],[1,1,1,4],[1,1,2,3],[1,2,2,2],[1,1,1,1,3],[1,1,1,2,2],[1,1,1,1,1,2],[1,1,1,1,1,1,1]]
  [n:7,len:15]
  [[8],[1,7],[2,6],[3,5],[4,4],[1,1,6],[1,2,5],[1,3,4],[2,2,4],[2,3,3],[1,1,1,5],[1,1,2,4],[1,1,3,3],[1,2,2,3],[2,2,2,2],[1,1,1,1,4],[1,1,1,2,3],[1,1,2,2,2],[1,1,1,1,1,3],[1,1,1,1,2,2],[1,1,1,1,1,1,2],[1,1,1,1,1,1,1,1]]
  [n:8,len:22]
  [n:9,len:30]
  [n:10,len:42]
  [n:11,len:56]
  [n:12,len:77]
  [n:13,len:101]
  [n:14,len:135]
  [n:15,len:176]
  [n:16,len:231]
  [n:17,len:297]
  [n:18,len:385]
  [n:19,len:490]
  [n:20,len:627]
  [n:21,len:792]
  [n:22,len:1002]
  [n:23,len:1255]
  [n:24,len:1575]
  [n:25,len:1958]
  [n:26,len:2436]
  [n:27,len:3010]
  [n:28,len:3718]
  [n:29,len:4565]
  [n:30,len:5604]
  [n:31,len:6842]
  [n:32,len:8349]
  [n:33,len:10143]
  [n:34,len:12310]

*/
go2 :-
        N in 2..34,
        indomain(N),
        findall(X, allpartitions(N, X),L),
        length(L, Len),
        % for larger N we really don't want to print all partitions
        (
         Len =< 22 -> writeln(L)
        ;
         true
        ),
        writeln([n:N, len:Len]),
        fail.

%
% The first part is simply to get an ordered array where the sum is N.
% This ordering is needed for removing symmetries when all 0's is removed.
% The second part removes 0 from the array.
%
allpartitions(N, Xs) :-
        % part I: get all candidates
        length(X, N),
        X ins 0..N,
        sum(X,#=,N),
        increasing(X), % Symmetry breaking
        label(X),
        
        % part II: for presentation, remove all 0's from X
        delete(X, 0, Xs). 
