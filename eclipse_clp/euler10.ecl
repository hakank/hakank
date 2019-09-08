
/* 
  Problem 10
http://projecteuler.net/index.php?section=problems&id=10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

Solution: 142913828922
Using is_prime3/1 it takes 7.3 seconds on the 64-bit machine.

*/

:- lib(ic).
:- lib(propia).
:- lib(hash).


is_prime3(2).
is_prime3(3).
is_prime3(P) :- integer(P), P > 3, P mod 2 =\= 0, \+ has_factor3(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.
%    (integer, integer) (+,+)

has_factor(N,L) :- N mod L #= 0.
has_factor(N,L) :- L * L #< N, L2 #= L + 2, has_factor(N,L2).

has_factor3(N,L) :- N mod L =:= 0.
has_factor3(N,L) :- L * L < N, L2 is L + 2, has_factor3(N,L2).


% 7.6s
problem10_tmp :-
        ( for(I, 1, 2000000), 
          fromto(0, In, Out, Sum)
        do 
          is_prime3(I)
        ->
          Out is In+I
        ; 
          Out=In
        ), 
        writeln(Sum).


% (7.37s)
problem10 :-
        problem10_tmp. % infers ic.


is_prime(P,Primes) :-
  (hash_contains(Primes,P) ; 
    (is_prime3(P),
     hash_add(Primes,P,1))
  ).
      

% Hashing the primes
% (8.89s)
problem10b_tmp :-
        hash_create(Primes),
        (
            for(I, 1, 2000000), 
            fromto(0, In, Out, Sum),
            param(Primes)
        do 
            is_prime(I,Primes)
        ->
            Out is In+I
        ; 
            Out=In
        ), 
        writeln(Sum).

% with infers ic: 8.633s.
% without infers ic: 8.66s.
problem10b :-
        problem10b_tmp. % infers ic.



go :-
        writeln('problem10'),
        problem10.
        % writeln('problem10_tmp'),
        % problem10_tmp.
        % writeln('problem10b'),
        % problem10b.
        % writeln('problem10b_tmp'),
        % problem10b_tmp.
        

