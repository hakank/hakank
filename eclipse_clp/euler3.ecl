/*
http://projecteuler.net/index.php?section=problems&id=3
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

?- problem3.
6857

*/

:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(ic_search).


% P31 (**) Determine whether a given integer number is prime. 
% is_prime(P) :- P is a prime number
%    (integer) (+)

is_prime(2).
is_prime(3).
% is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  
is_prime(P) :- integer(P), P #> 3, P mod 2 #\= 0, \+ has_factor(P,3).  

is_prime3(2).
is_prime3(3).
is_prime3(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor3(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L #= 0.
has_factor(N,L) :- L * L #< N, L2 #= L + 2, has_factor(N,L2).

has_factor3(N,L) :- N mod L =:= 0.
has_factor3(N,L) :- L * L < N, L2 is L + 2, has_factor3(N,L2).

% P35 (**) Determine the prime factors of a given positive integer. 
% prime_factors(N, L) :- N is the list of prime factors of N.
%    (integer,list) (+,?)

prime_factors(N,L) :- N #> 0,  prime_factors(N,L,2).

% prime_factors(N,L,K) :- L is the list of prime factors of N. It is 
% known that N does not have any prime factors less than K.
prime_factors(1,[],_) :- !.
prime_factors(N,[F|L],F) :-   % N is multiple of F
        %   R is N // F, N =:= R * F, !, prime_factors(R,L,F).
        R #= N // F, N #= R * F, !, prime_factors(R,L,F).
prime_factors(N,L,F) :- 
        next_factor(N,F,NF), prime_factors(N,L,NF).        % N is not multiple of F
   

%
% this use a more direct method than is_prime
%
is_prime2(2).
is_prime2(3).

is_prime2(N) :-
        N #\= 1,
        Mod2 is N mod 2,
        Mod2 #\= 0,
        Max is integer(ceiling(sqrt(N))),
        (for(I,3,Max,2), param(N) do
             T is N mod I,
             T #\= 0
        ).


%
% This is much faster than prime_factors/2
%
prime_factors3(N,L) :- N > 0,  prime_factors3(N,L,2).
prime_factors3(1,[],_) :- !.
prime_factors3(N,[F|L],F) :-   % N is multiple of F
        R is N // F, N =:= R * F, !, prime_factors3(R,L,F).
prime_factors3(N,L,F) :- 
        next_factor3(N,F,NF), prime_factors3(N,L,NF).        % N is not multiple of F

% next_factor(N,F,NF) :- when calculating the prime factors of N
%    and if F does not divide N then NF is the next larger candidate to
%    be a factor of N.

next_factor(_,2,3) :- !.
% next_factor(N,F,NF) :- F * F < N, !, NF is F + 2.
next_factor(N,F,NF) :- F * F #< N, !, NF #= F + 2.
next_factor(N,_,N).                                 % F > sqrt(N)

next_factor3(_,2,3) :- !.
next_factor3(N,F,NF) :- F * F < N, !, NF is F + 2.
next_factor3(N,_,N).                                 % F > sqrt(N)

%
% From http://rosettacode.org/wiki/Prime_decomposition#Prolog
% This is faster than prime_factors3/2.
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


% From SWI lists.pl
max_list([B|A], C) :-
        max_list(A, B, C).

max_list([], A, A).
max_list([A|C], B, E) :-
        D is max(A, B),
        max_list(C, D, E).

% ok (0.03s)
problem3a :-
        prime_factors(600851475143,X), 
        max_list(X, Max),
        writeln(Max).


% problem3b :- prime_factors2(600851475143,X), 
%         max_list(X, Max),
%         writeln(Max).

% Fast (0.00s)
problem3c :- prime_factors3(600851475143,X), 
        max_list(X, Max),
        writeln(Max).

% Fast
problem3d :- prime_decomp(600851475143,X), 
        max_list(X, Max),
        writeln(Max).


% This is ECLiPSi:er ...
prime_factors4(N,Factor)  :-
        Max is integer(ceiling(sqrt(N))) + 1,
        (for(I,2,Max),
         fromto(Factor,Out,In,[]),param(N) do
             (
             R is N mod I,
             R == 0, 
             is_prime2(I)
             )  -> 
             Out = [I|In]
        ;
             Out = In
        ).
           
% ok (0.11s)
problem3 :-
        prime_factors4(600851475143,Factors), 
        % prime_factors(600851475143,Factors), 
        Max is max(Factors),
        writeln(Max).


go :-
        writeln('problem3'),
        problem3,
        writeln('problem3a'),
        problem3a,
        writeln('problem3c'),
        problem3c,
        writeln('problem3d'),
        problem3d.