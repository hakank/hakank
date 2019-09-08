% Problem 14
%
% """
% The following iterative sequence is defined for the set of positive integers:
%
% n n/2 (n is even)
% n 3n + 1 (n is odd)
%
% Using the rule above and starting with 13, we generate the following 
% sequence:
% 13 40 20 10 5 16 8 4 2 1
%
% It can be seen that this sequence (starting at 13 and finishing at 1) 
% contains 10 terms. Although it has not been proved yet (Collatz
% Problem), it is thought that all starting numbers finish at 1.
%
% Which starting number, under one million, produces the longest chain?
%
% NOTE: Once the chain starts the terms are allowed to go above one million.")
% """

:- lib(lists).
:- lib(listut).
:- lib(hash).
:- lib(propia).

%
% Answer: [max : 525, nth : 837798]
% Brute force version: 31.8s
% Using cache: 10.7s (though I had to increase the global 
% stack space, see below).
%
% See http://eclipseclp.org/doc/bips/kernel/compiler/inline-2.html
% Though, it don't speed it up...
% :- inline(collatz/2). 
collatz(1,1) :- !.
collatz(N,C) :-
        N > 1,
        N mod 2 =:= 0, !,
        C is N // 2.
collatz(N,C) :-
        N > 1,
        N mod 2 \= 0, !,
        C is 1+3*N.

collatz_list(1,[1]) :- !.
collatz_list(N,[N|Res]) :-
        collatz(N,C),
        collatz_list(C,Res).

% (31.8s)
problem14 :-
        N = 1000000,
        ( for(I,1,N),
          fromto(Lens,Out,In,[]) do
              collatz_list(I,List),
              length(List,Len),
              Out = [Len|In]
        ),
        eclipse_language:max(Lens,Max),
        nth1(Nth,Lens,Max),!,
        writeln([max:Max,nth:Nth]).


collatz_list3(1,1,_) :- !.
collatz_list3(N, Len2, CollatzHash) :-
        N > 1,
        (
        hash_get(CollatzHash, N, Val) ->
            Len2 is Val-1
        ;
            collatz(N,C),
            Len2 is Len + 1,
            collatz_list3(C, Len, CollatzHash),
            hash_add(CollatzHash, C, Len2)
        ).
        
% Another approach using a hash table for memoizing the values.
%
% This is faster than problem14 (at least on N=100000).
% (10.7s)
problem14b :-
        % writeln('Please start with eclipse -g 500M -b euler14.ecl'), % not needed
        hash_create(CollatzHash),
        N = 1000000,
        ( for(I,1,N),
          fromto(Lens,Out,In,[]),
          param(CollatzHash) do
              collatz_list3(I, Len, CollatzHash), % correct and faster (10.7s)
              Out = [Len|In]
        ),
        eclipse_language:max(Lens,Max),
        nth1(Nth,Lens,Max),!,
        writeln([max:Max,nth:Nth]).


go :-
        % writeln("problem14"),
        % problem14,
        writeln("problem14b"),
        problem14b.