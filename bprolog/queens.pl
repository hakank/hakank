/*

  N-queens in B-Prolog.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

/*
  More systematic test of queens2/2 and queens/2.

  Both queens2/2 and queens/2 take long time for N=500
  (but is much faster for N=499 and N=500:
  For N=400:
    queens2/2: 0.62s 10 backtracks
    queens/2:  2.16s 1 backtrack
  For N=499:
    queens2/2: 0.76 1 backtracks
    queens/2:  4.168 0 backtracks
  For N=500: too slow
  For N=501:
    queens2/2: 3.14s 1 backtrack
    queens/2:  3.524s 2 backtracks

  For N=1000:
    queens2/2: 24.2s 2 backtracks
    queens/2:  34.146s 0 backtracks
    
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


% Decomposition of alldifferent
alldifferent_me(L) :-
        length(L, Len),
        foreach(I in 1..Len, J in I+1..Len, L[I] #\= L[J]).

%
% Decomposition of alldifferent for ip_solve
% (used in queens7/2).
%
alldifferent_mip(L) :-
        length(L, Len),
        foreach(I in 1..Len, J in I+1..Len, L[I] $\= L[J]).


%
% Testing correctness of queens/2 (using alldistinct)
%
go :-
        N = 8,
        findall(Q, queens(N,Q), L),
        length(L,Len),
        writeln(L),
        writeln(len:Len).

% Larger example
go2 :-
        N = 300,
        time2(queens(N,Q)),
        writeln(Q).

% Testing queens2/2.
% This is quite hard. > 35 minutes...
go3 :-
        N = 500,
        time2(queens2(N,Q)),
        writeln(Q).

% Testing queens3/2
go4 :-
        N = 8,
        time2(queens3(N,Q)),
        writeln(Q).

go5 :-
        Sizes = [8,10,20,100,200,300,400,499,501],
        foreach(N in Sizes,
                [Q,Q2],
                (
                  garbage_collect,
                  writeln(n:N),
                  % time2(queens5(N, Q)),
                  % writeln(queens5:Q),
                  time2(queens(N, Q)),
                  writeln(queens:Q),
                  time2(queens2(N, Q2)),
                  writeln(queens2:Q2),
                  nl
                )
               ).

% Using the decomposition of alldifferent.
% Testing correctness.
go6 :-
        N = 8,
        findall(Q, queens4(N,Q), L),
        length(L,Len),
        writeln(L),
        writeln(len:Len).

% Using the decomposition of alldifferent
go7 :-
        N = 50,
        time2(queens4(N,Q)),
        writeln(Q).

%
% Systematic test of queens4/2 (decomposition of alldifferent)
%
go8 :-
        foreach(N in 8..5..100,
                [Q],
                (
                  writeln(n:N),
                  time2(queens4(N, Q)),
                  writeln(Q),
                  nl
                )
               ).


%
% Using SAT solver (sat_solve)
% Not blazingly fast:
%  N=50: 3.18s
%  N=100: 25.66s
%
go9 :-
        findall(Q, queens6(8,Q), All),
        writeln(All),
        length(All, Len),
        writeln(len:Len),
        time2(queens6(100,Q2)),
        writeln(Q2).

%
% Using IP solver (ip_solve)
%
go10 :-
        time2(queens7(8,Q)),
        writeln(Q).

go11 :-
   foreach(N in 8..15, 
         [Count],
         (
          writeln(n=N),
          count(queens3(N,X),Count),
          writeln(N=Count)
         )
   ),
   nl.




% From Albert Dinero <albertmcchan@yahoo.com
count(Goal, N) :-
  global_set(count, 0),
  (
  Goal,
  global_get(count, N),
  N1 is N + 1,
  writeln(n1=N1),
  global_set(count, N1),
  fail;
  global_get(count, N)
  ).



%
% Traditional: using 3 alldistinct.
%
queens(N, Q) :-
        length(Q, N),
        Q :: 1..N,
        % Note: We must "extract" via @=
        % This don't_ work: alldistinct([Q[I]+I : I in 1..N])

        Q2 @= [Q[I]+I : I in 1..N],
        Q3 @= [Q[I]-I : I in 1..N],
        alldistinct(Q),
        alldistinct(Q2),
        alldistinct(Q3),
        
        labeling([ff],Q).


%
% Traditional: Using alldifferent instead of alldistinct
% is little faster than using alldistinct.
%
queens2(N, Q) :-
        length(Q, N),
        Q :: 1..N,

        Q2 @= [Q[I]+I : I in 1..N],
        Q3 @= [Q[I]-I : I in 1..N],
        alldifferent(Q),
        alldifferent(Q2),
        alldifferent(Q3),
        
        labeling([ff],Q).

%
% This is - unsurprisingly - much slower.
%
queens3(N, Q) :-
        length(Q, N),
        Q :: 1..N,
        foreach(I in 1..N, J in I+1..N,
                (
                  Q[I] #\= Q[J],
                  Q[I] + I #\= Q[J],
                  Q[I] - I #\= Q[J]
                ;
                  true
                )
               ),
        labeling([ff], Q).


%
% Using decomposition of alldifferent. Slower.
%
queens4(N, Q) :-
        length(Q, N),
        Q :: 1..N,
        Q2 @= [Q[I]+I : I in 1..N],
        Q3 @= [Q[I]-I : I in 1..N],
        
        alldifferent_me(Q),
        alldifferent_me(Q2),
        alldifferent_me(Q3),
        
        labeling([ffd],Q).


%
% slightly different approach (from queens/2 and queens2/2)
% where we start with an array A and then extract the
% lists Q from A.
%
queens5(N, Q) :-
        new_array(A,[N]),
        % Q @= [A[I] : I in 1..N],
        array_to_list(A,Q),
        Q :: 1..N,
        Q2 @= [A[I]+I : I in 1..N],
        Q3 @= [A[I]-I : I in 1..N],
        
        % alldifferent(Q),
        % alldifferent(Q2),
        % alldifferent(Q3),

        alldistinct(Q),
        alldistinct(Q2),
        alldistinct(Q3),

        
        % labeling([ff],Q).
        labeling([ff],Q).


%
% SAT based solution
% Note: This only generate one solution
%
queens6(N, Q) :-
        length(Q, N),
        Q :: 1..N,

        Q2 @= [Q[I]+I : I in 1..N],
        Q3 @= [Q[I]-I : I in 1..N],
        % Special constraints for SAT solve
        $alldifferent(Q),
        $alldifferent(Q2),
        $alldifferent(Q3),
        
        sat_solve(Q).


%
% LP based solution
% Note: This only generate one solution.
% Time:
%   N=8: 2.56s
%   N=10: 61.6s
%
queens7(N, Q) :-
        length(Q, N),
        Q :: 1..N,
        foreach(QQ in Q, lp_integer(QQ)),
        Q2 @= [Q[I]+I : I in 1..N],
        Q3 @= [Q[I]-I : I in 1..N],
        
        % Using $alldifferent/1 don't work
        % (GLPK give PROBLEM HAS NO FEASIBLE SOLUTION)
        % $alldifferent(Q),
        % $alldifferent(Q2),
        % $alldifferent(Q3),
        
        % This works, though.
        alldifferent_mip(Q),
        alldifferent_mip(Q2),
        alldifferent_mip(Q3),       
        ip_solve(Q).



%
% Collecting all the alldifference in a foreach loop.
% However it's slower than queens/2 and queens2/2...
%
queens8(N, Q) :-
        length(Q, N),
        Q :: 1..N,
        foreach(A in [-1,0,1],
                [QQ],
                (
                  QQ @= [Q[I]+I*A : I in 1..N],
                  alldifferent(QQ)
                )
               ),
        labeling([ffd],Q).
