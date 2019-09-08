%
% Problem 25
%
% """
% The Fibonacci sequence is defined by the recurrence relation:
%
%    Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
% 
% Hence the first 12 terms will be:
%
%    F1 = 1
%    F2 = 1
%    F3 = 2
%    F4 = 3
%    F5 = 5
%    F6 = 8
%    F7 = 13
%    F8 = 21
%    F9 = 34
%    F10 = 55
%    F11 = 89
%    F12 = 144
%
% The 12th term, F12, is the first term to contain three digits.
%
% What is the first term in the Fibonacci sequence to contain 1000 digits?")
% """
%
% Answer: 4782
% Best version: 0.08s
% 

:- lib(listut).

go :-
  problem25.

go2 :-
  problem25b.

go3 :-
  problem25c.


% From 
% http://coding.derkeiler.com/Archive/Prolog/comp.lang.prolog/2003-11/0188.html
%
fib(0,1).
fib(1,1).
fib(N,F) :- N > 1, fib(N,1,1,F).

fib(2,F1,F2,F) :- F is F1 + F2.
fib(N,F1,F2,F) :- N > 2, N1 is N - 1, NF1 is F1 + F2,
    fib(N1,NF1,F1,F). 

fib_len(1,1) :- !.
fib_len(2,1) :- !.
fib_len(N,Len) :-
        fib(N,Fib),
        name(Fib,FibStr),
        length(FibStr,Len), !.

% fib_while(1,1) :- !.
fib_while(N,Limit,Result) :-
        fib_len(N,Len),
        % writeln(fib_while:[N,Len]),
        (
        Len < Limit -> 
            N1 is N+1,
            fib_while(N1, Limit,_Len2)
        ;
            Winner is N+1,
            Result is Winner,!,
            writeln(winner:Winner)
        ).

% It seems that this don't like numbers with a length > 309.
numlen(N,Len) :-
        Len is 1+integer(floor(ln(N)/ln(10))).

% Another take (quite fast but still just result with writeln)
% TODO: Ensure that the result is in the Result variable
fib_while2(N,List,Limit,Result) :-
        List = [Last1,Last2|_Rest],
        Next is Last1 + Last2,
        name(Next,NextStr),
        length(NextStr,NextLen),
        % numlen(Next,NextLen), % don't work here
        N1 is N+1,
        (
            NextLen < Limit -> 
                fib_while2(N1, [Next|List],Limit,NextLen)
        ;
                writeln(winner:N),
                Result is NextLen,
                fib_while2(N1, [Next|List],Limit,Result),!
        ).


% This is a "proper" solution and also quite fast (0.08s):  
% It returns the list of all lengths in the last argument 
% (Result); the answer is the last element (see problem25c).
fib_while3(N,List,Limit,[N|Result]) :-
        List = [Last1,Last2|_Rest],
        Next is Last1 + Last2,
        name(Next,NextStr),
        length(NextStr,NextLen),
        N1 is N+1,
        NextLen < Limit,
        fib_while3(N1,[Next|List],Limit,Result), !.

fib_while3(N,_List,_Limit,[N]).

last_element(List,Last) :-
        length(List,Len),
        nth1(Len,List,Last).


% Faster (0.09s)
% Don't return any thing in Result
problem25 :-
        fib_while2(3,[1,1],1000,Result),
        last_element(Result,Winner),
        writeln(result:Winner).

% Slower (2.7s)
% Don't return any thing in Result
problem25b :-
        fib_while(0,1000,Result),
        writeln(Result).

% 0.08s (and a "proper" solution since we get the result via List)
problem25c :-
        fib_while3(3,[1,1],1000,List),
        last_element(List,Result),
        writeln(Result).
