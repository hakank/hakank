/*

Problem 1

http://projecteuler.net/index.php?section=problems&id=1

If we list all the natural numbers below 10 that are multiples of 3 
or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

Answer: 233168

*/

go :-
        L = [euler1a,
             euler1a2,
             euler1b,
             euler1c,
             euler1d,
             euler1d2,
             euler1e,
             euler1f,
             euler1g,
             euler1h,
             euler1i,
             euler1j
            ],
        foreach(E in L, proc(E)).

proc(E) :-
        write(E),write(':'),
        time(call(E)), nl.


% Using list comprehension
euler1a :-
       L @= [I:I in 1..999,(I mod 3=:=0;I mod 5=:=0)],
       sumlist(L,Sum),
       writeln(Sum).

% With sum/1 directly on the list comprehension.
% Note: it must be Sum #= ... (or Sum $= ...),
% using Sum is ... don't work
% 
euler1a2 :-
       Sum#=sum([I:I in 1..999,(I mod 3=:=0;I mod 5=:=0)]),
       writeln(Sum).


% Using foreach with accumulators. Not as nice as euler1.
euler1b :-
         foreach(I in 1..999,ac(S,0),
         (
           ((I mod 3 =:=0;I mod 5=:=0)->S^1 is S^0+I)
         ; 
           S^1 is S^0)
         ),
         writeln(S).


% plain Prolog
euler1c :-
        euler1c_pred(1, 999, 0, Res),
        writeln(Res).

euler1c_pred(N, Limit, R, R) :- N > Limit.
euler1c_pred(N, Limit, R, R2) :-
        (
          (N mod 3 =:= 0; N mod 5 =:= 0)
        ->
          R1 is R + N
        ;
          R1 = R
        ),
        % N2 is N + 1, % Not needed
        euler1c_pred(N+1, Limit, R1,R2).


p(N,N) :- N mod 3 =:= 0, !.
p(N,N) :- N mod 5 =:= 0, !.
p(_,0).

% same principle as euler1 but using predicate p/2.
euler1d :-
        L @= [C : N in 1..999, [C], p(N, C)],
        sumlist(L,Sum),
        writeln(Sum).


% sum(list comprehension)
euler1d2 :-
        Sum #= sum([C : N in 1..999, [C], p(N, C)]),
        writeln(Sum).


% using findall
% This seems to be the fastest: 0.0s
euler1e :-
        L @= [I : I in 1..999],
        findall(C, (member(N, L), p(N,C)),Res),
        Sum is sum(Res),
        writeln(Sum).


% Using CLP(FD), though it's slower (0.152s instead of 0.004s)
euler1f :-
        euler1f_pred(1, 999, 0, Res),
        writeln(Res).

euler1f_pred(N, Limit, R, R) :- N #> Limit.
euler1f_pred(N, Limit, R, R2) :-
        ((N mod 3 #= 0 #\/ N mod 5 #= 0) -> R1 #= R + N ; R1 #= R),
        euler1f_pred(N+1, Limit, R1,R2).


% Purer CLP(FD), a little faster than euler1f (0.135s) but slower than
% the other
euler1g :-
        Len = 999,
        length(L, Len),
        L in 0..Len, 
        foreach(N in 1..Len, L[N] #= N*((N mod 3 #= 0 #\/ N mod 5 #= 0))),
        labeling([ff], L),
        sumlist(L, Sum),
        writeln(Sum).

% slightly different from euler1g but faster (0.008s)
% Note that we don't have to use explicit labeling here
euler1h :-
        Len = 999,
        length(L, Len),
        L @= [R : N in 1..Len, [R], R #= N*((N mod 3 #= 0 #\/ N mod 5 #= 0))],
        sumlist(L, Sum),
        writeln(Sum).


% As euler1h but with sum directly on the list comprehension
euler1i :-
        Len = 999,
        Sum #= sum([R : N in 1..Len, [R], R #= N*((N mod 3 #= 0 #\/ N mod 5 #= 0))]),
        writeln(Sum).


% Variant of euler1d with another test function (p2/1 instead of p/2).
p2(N) :- N mod 3 =:= 0; N mod 5 =:= 0.
euler1j :-
        Sum #= sum([N : N in 1..999, p2(N)]),
        writeln(Sum).


% using sat_solve (don't work)
% euler1i :-
%         Len = 999,
%         L @= [N*((N mod 3 $= 0 $\/ N mod 5 $= 0)) : N in 1..Len],
%         sumlist(L, Sum),
%         sat_solve(L),
%         writeln(Sum).