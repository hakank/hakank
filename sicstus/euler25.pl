/*

  Euler problem 25 in SICStus Prolog

  """
  The Fibonacci sequence is defined by the recurrence relation:

     Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
  
  Hence the first 12 terms will be:

     F1 = 1
     F2 = 1
     F3 = 2
     F4 = 3
     F5 = 5
     F6 = 8
     F7 = 13
     F8 = 21
     F9 = 34
     F10 = 55
     F11 = 89
     F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?")
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstu_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
            % euler25a,
            % euler25b,
            euler25c
             ],
        run_problems(L).

%%
%% 1.803s
%%
euler25a :-
        fib_while(1,1000).

%%
%% 0.123s
%%
euler25b :-
        fib_while2(3,[1,1],1000,List),
        last(List,Result),
        writeln(Result).

%%
%% 0.106s
%%
euler25c :-
        fib_while3(3,[1,1],1000,List),
        last_element(List,Result),
        writeln(Result).


%%
%% fib_length(N,F,Len)
%%
%% F is the N'th Fibonacci number.
%% Len is the length of the N'th Fibonacci number,
%%
fib_length(N,F,Len) :-
    fib_memo(N,F),
    number_chars(F,L),
    length(L,Len).

%%
%% number_length(N,Len)
%%
%% Len is the length of integer N.
%% This is much slower than fib_length.
%% Throws overflow on larger numbers (> length of 309)
%% and is thus useless for this problem.
%%
number_length(N,Len) :-
        Len is 1+floor(log(10,N)).


%%
%% Fibonacci loop.
%% The predicate just print the answer, i.e. there's
%% no output value.
%%
fib_while(N,Limit) :-
        fib_length(N,_F,Len),
        (
         Len < Limit
        -> 
         N1 is N+1,
         fib_while(N1, Limit)
        ;
         Winner is N+1,
         writeln(Winner)
        ).

%%
%% Another Fibonacci loop.
%%
%% The answer is in the last element of Result.
%% 
fib_while2(N,List,Limit,[N|Result]) :-
        List = [Last1,Last2|_Rest],
        Next is Last1 + Last2,
        name(Next,NextStr),
        length(NextStr,NextLen),
        N1 is N+1,        
        ( NextLen < Limit
        ->
          fib_while2(N1,[Next|List],Limit,Result)
        ;
          true
        ).

% This is a "proper" solution and also quite fast (0.08s):  
% It returns the list of all lengths in the last argument 
% (Result); the answer is the last element.
fib_while3(N,List,Limit,[N|Result]) :-
        List = [Last1,Last2|_Rest],
        Next is Last1 + Last2,
        name(Next,NextStr),
        length(NextStr,NextLen),
        N1 is N+1,
        NextLen < Limit,
        fib_while3(N1,[Next|List],Limit,Result), !.

fib_while3(N,_List,_Limit,[N]).