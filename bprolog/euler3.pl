/*
http://projecteuler.net/index.php?section=problems&id=3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

?- problem3.
6857

*/

go :-
        L = [
             euler3a,
             euler3b,
             euler3c,
             euler3d
            ],
        foreach(E in L, proc(E)).

proc(E) :-
        write(E),write(': '),
        time(call(E)), nl.

is_prime(N) :-
        L @= [J: J in 2..sqrt(N), [M], (M is N mod J, M =:= 0)],
        L = [].

:- table is_prime2/1.
is_prime2(N) :-
        divisors(N, Divisors),
        Divisors = [].

divisors(N, Div) :-
        Div @= [J: J in 2..sqrt(N), [M], (M is N mod J, M =:= 0)].


prime_divisors(N, Div) :-
        Div @= [J: J in 2..sqrt(N), [M], (M is N mod J, M =:= 0, is_prime2(J))].


prime_factors(N,L) :- N #> 0,  prime_factors(N,L,2).



prime_factors(N,L) :- N > 0,  prime_factors3(N,L,2).
prime_factors(1,[],_) :- !.
prime_factors(N,[F|L],F) :-   % N is multiple of F
        R is N // F, N =:= R * F, !, prime_factors(R,L,F).
prime_factors(N,L,F) :- 
        next_factor(N,F,NF), prime_factors(N,L,NF). % N is not multiple of F


% next_factor(N,F,NF) :- when calculating the prime factors of N
%    and if F does not divide N then NF is the next larger candidate to
%    be a factor of N.
next_factor(_,2,3) :- !.
next_factor(N,F,NF) :- F * F < N, !, NF is F + 2.
next_factor(N,_,N).


%
% From http://rosettacode.org/wiki/Prime_decomposition#Prolog
%
% hakank: I had to add integer/1, floor/1, and // (instead of /)
%
prime_decomp(N, L) :-
	SN is integer(floor(sqrt(N))),
	prime_decomp_1(N, SN, 2, [], L).
 
prime_decomp_1(1, _, _, L, L) :- !.

% Special case for 2, increment 1
prime_decomp_1(N, SN, D, L, LF) :-
	(   0 is N mod D ->
	    Q is N // D,
	    SQ is integer(floor(sqrt(Q))),
	    prime_decomp_1(Q, SQ, D, [D |L], LF)
	;
	    D1 is D+1,
	    (	D1 > SN ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).
 
% General case, increment 2
prime_decomp_2(1, _, _, L, L) :- !.
 
prime_decomp_2(N, SN, D, L, LF) :-
	(   0 is N mod D ->
	    Q is N // D,
	    SQ is integer(floor(sqrt(Q))),
	    prime_decomp_2(Q, SQ, D, [D |L], LF);
	    D1 is D+2,
	    (	D1 > SN ->
	        LF = [N |L]
	    ;
	        prime_decomp_2(N, SN, D1, L, LF)
	    )
	).

      
% not fast: 3.99s
euler3a :-
        divisors(600851475143, Divisors),
        Factors @= [D : D in Divisors, is_prime2(D)],
        Max #= max(Factors),
        writeln(Max).

% even slower: 4.57s
euler3b :-
        prime_divisors(600851475143, Factors),
        Max #= max(Factors),
        writeln(Max).


% Fast (0.004s)
euler3c :-
        prime_factors(600851475143,Factors), 
        Max #= max(Factors),
        writeln(Max).

% Fastest (0.0s)
euler3d :-
        prime_decomp(600851475143,Factors), 
        Max #= max(Factors),
        writeln(Max).

