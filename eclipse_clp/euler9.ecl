
/*
  Problem 9

http://projecteuler.net/index.php?section=problems&id=9

A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

Solution: 31875000

*/

:- lib(ic).
:- lib(propia).

%
% Why does it leave a suspended goal?
%
% Note: this goal is faster:
%   problem9_tmp infers most.
%
triplet([A, B, C], Prod) :-
     LD = [A,B,C],
     LD :: 1..500,
     A + B + C #= 1000,
     A #=< B, % symmetry breaking (with infers most we don't need this)
     B #=< C, 
     Prod #= A * B *C,
     A^2 + B^2 - C^2 #= 0,
     search(LD, 0, first_fail, indomain_max, complete, []).

% This gives delays... (0.13s)
problem9_tmp :- 
        triplet(L, Prod),
        writeln([L,Prod]) .

problem9b:-
        problem9_tmp infers most.        

% No delays here... (0.27s)
problem9 :-
        findall([Vars, Product], triplet(Vars,Product), Result),
        writeln(Result).


go :-
        writeln('problem9'),
        problem9,
        writeln('problem9b'),
        problem9b,
        writeln('problem9_tmp'),
        problem9_tmp.