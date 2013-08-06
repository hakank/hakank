/*

  Least diff problem in B-Prolog.

  The model solves the following problem:
 
  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  Minimize the difference 
    ABCDE - FGHIJ


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
        time2(least_diff([L,X,Y,Diff], Backtracks)),
        writeln(L),
        format('~d - ~d = ~d\n',[X,Y,Diff]),
        format('~d backtracks\n',[Backtracks]).

go2 :-
        time2(least_diff2([L,X,Y,Diff], Backtracks)),
        writeln(L),
        format('~d - ~d = ~d\n',[X,Y,Diff]),
        format('~d backtracks\n',[Backtracks]).


%
% A small benchmark:
% least_diff2/2 is slightly faster than least_diff/2. 
% For N=1000
%  least_diff : 10.916s
%  least_diff2: 10.009s
%
go3 :-
        N = 1000,
        writeln(first_diff),
        time(
        foreach(_ in 1..N,
                [L,X,Y,Diff, Backtracks],
                least_diff([L,X,Y,Diff],Backtracks))
             ),

        writeln(first_diff2),
        time(
        foreach(_ in 1..N,
                [L,X,Y,Diff, Backtracks],
                least_diff2([L,X,Y,Diff],Backtracks))
             ).



%
% Using scalar_product/4.
%
least_diff([L,X,Y,Diff], Backtracks) :-
        statistics(backtracks, Backtracks1),
        L = [A,B,C,D,E,F,G,H,I,J],
        L :: 0..9,
        length(L, N),
        M is N // 2,

        alldifferent(L),
        % Since 10**0 yield 1.0 instead of 1, we have to use integer/1.
        Base @= [T : I in 1..M, [T], T is integer(10**(M-I))],
        scalar_product(Base,[A,B,C,D,E],#=,X),
        scalar_product(Base,[F,G,H,I,J],#=,Y),
        Diff #= X - Y,
        Diff #> 0,

        minof(labeling([ff,down],L), Diff),

        statistics(backtracks, Backtracks2),
        Backtracks is Backtracks2 - Backtracks1.


%
% Different approach in the arithmetics
%
least_diff2([L,X,Y,Diff], Backtracks) :-
        statistics(backtracks, Backtracks1),
        L = [A,B,C,D,E,F,G,H,I,J],
        L :: 0..9,

        alldifferent(L),
        X #= 10000*A + 1000*B + 100*C + 10*D + E,
        Y #= 10000*F + 1000*G + 100*H + 10*I + J,
        Diff #= X - Y,
        Diff #> 0,

        minof(labeling([ff,down],L), Diff),

        statistics(backtracks, Backtracks2),
        Backtracks is Backtracks2 - Backtracks1.
        

% sat_solve
% (if it's a solution it's > 5minutes)
least_diff3(L,X,Y,Diff) :-
        L = [A,B,C,D,E,F,G,H,I,J],
        L :: 0..9,

        $alldifferent(L),
        X $= 10000*A + 1000*B + 100*C + 10*D + E,
        Y $= 10000*F + 1000*G + 100*H + 10*I + J,
        Diff $= X - Y,
        Diff $> 0,

        sat_solve([min(Diff)], L).
        

%
% Decomposition of alldifferent for ip_solve
%
alldifferent_mip(L) :-
        length(L, Len),
        foreach(I in 1..Len, J in I+1..Len, L[I] $\= L[J]).


% ip_solve
least_diff4(L,X,Y,Diff) :-
        L = [A,B,C,D,E,F,G,H,I,J],
        L :: 0..9,
        lp_domain(L,0,9),
        lp_integers(L),

        foreach(LL in L, lp_domain(LL,0,9)),
        foreach(LL in L, lp_integer(LL)),

        alldifferent_mip(L),
        X $= 10000*A + 1000*B + 100*C + 10*D + E,
        Y $= 10000*F + 1000*G + 100*H + 10*I + J,
        Diff $= X - Y,
        Diff $> 0,

        % ip_solve([dump, min(Diff)], L).
        ip_solve([min(Diff)], L).
        
