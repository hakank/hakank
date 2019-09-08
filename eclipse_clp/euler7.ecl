
/*

  problem7
http://projecteuler.net/index.php?section=problems&id=7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number?

Solution: 104743

Aside: This lists the primes between 1 and 100000
[eclipse 16]: (for(I, 1, 100000), fromto(List,Out,In,[]) do is_prime(I) -> Out=[I|In]; Out=In), length(List, Len).

I = I
List = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, ...]
Out = Out
In = In
Len = 9592
Yes (1.28s cpu)
*/

:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(propia).
:- lib(ic_search).
:- lib(hash).

is_prime3(2).
is_prime3(3).
is_prime3(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor3(P,3).  

has_factor3(N,L) :- N mod L =:= 0.
has_factor3(N,L) :- L * L < N, L2 is L + 2, has_factor3(N,L2).

% next_prime(2,P) -> P = 3, so we must first add 1.
next_prime(Num, P) :- Num2 is Num + 1, next_prime2(Num2, P).
next_prime2(Num, P) :- is_prime3(Num), !, P #= Num.
next_prime2(Num, P) :-
        Num2 #= Num+1,
        next_prime2(Num2,P).

nth_prime(Choosen, P) :-
        nth_prime(1,0,Choosen, P).

% nth_prime(Num, Choosen, Choosen, P) :- is_prime(Num), !, P #= Num.
nth_prime(Num, Choosen, Choosen, P) :- is_prime3(Num), !, P #= Num.
nth_prime(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 is Ix + 1,
        next_prime(Num, P2),
        % writeln(['Second:', Num, Ix, Choosen, P, P2]),
        nth_prime(P2, Ix2, Choosen, P).

% ok (quite slow)
% later: using is_prime3 makes it much faster (0.26s)
problem7 :-
        nth_prime(10001, P), labeling([P]), writeln(P).



go :-
        writeln('problem7'),
        problem7.