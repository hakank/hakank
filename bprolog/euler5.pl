/*

  Problem 5
  http://projecteuler.net/index.php?section=problems&id=5

  2520 is the smallest number that can be divided by each of the 
  numbers from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of 
  the numbers from 1 to 20?

  Answer: 232792560
*/

go :-
        L = [
             euler5a,
             euler5b

            ],
        foreach(E in L, proc(E)).


proc(E) :-
        write(E),write(': '),
        time(call(E)), nl.


% gcd/2 is a builtin (though not documented in the manual)
lcm(X,Y,LCM) :-
        GCD is gcd(X,Y),
        LCM is X*Y//GCD.


foldr(_Op, Init, [], Init).
foldr(Op, Init, [X|Xs], R) :- 
        foldr(Op, Init, Xs, R1), 
        P =.. [Op, X, R1, R], 
        call(P).

% 0.0s
euler5a :-
        List @= [I : I in 2..20],
        foldr(lcm, 1, List, As),
        writeln(As).

% using foreach with an accumulator
% 0.0s
euler5b :-
        foreach(I in 2..20,
                ac(Lcm, 1),
                lcm(Lcm^0,I,Lcm^1)
               ),
        writeln(Lcm).



