/*

  Euler problem 7
  
  http://projecteuler.net/index.php?section=problems&id=7

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
  the 6th prime is 13.

  What is the 10001st prime number?

  Solution: 104743

*/


go :-
        write('euler7: '),
        time(euler7).


is_prime(2).
is_prime(3).
is_prime(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor(P,3).  

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, L2 is L + 2, has_factor(N,L2).

next_prime(Num, P) :- Num2 is Num + 1, next_prime2(Num2, P).
next_prime2(Num, P) :- is_prime(Num), !, P #= Num.
next_prime2(Num, P) :-
        Num2 #= Num+1,
        next_prime2(Num2,P).

nth_prime(Choosen, P) :-
        nth_prime(1,0,Choosen, P).

nth_prime(Num, Choosen, Choosen, P) :- is_prime(Num), !, P #= Num.
nth_prime(Num, Ix, Choosen, P) :- 
        Ix #< Choosen,
        Ix2 is Ix + 1,
        next_prime(Num, P2),
        nth_prime(P2, Ix2, Choosen, P).

% 1.48s
euler7 :-
        nth_prime(10001, P), labeling([P]), writeln(P).
