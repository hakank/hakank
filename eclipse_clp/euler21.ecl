%
% Problem 21
% """
% Let d(n) be defined as the sum of proper divisors of n (numbers less 
% than n which divide evenly into n).
% If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable 
% pair and each of a and b are called amicable numbers.
% 
% For example, the proper divisors of 220 are 
% 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
% The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
% 
% Evaluate the sum of all the amicable numbers under 10000.
% """
%
% Answer: 31626
%
% 5.85s (using divisor/2 in divisors_proper/2) 
% 0.23s (using divisor2/2 in divisors_proper/2) 
% 

:- lib(util), lib(listut).

go :-
  problem21.

% From
% http://www.cs.washington.edu/education/courses/cse341/03sp/slides/PrologEx/lists.pl.txt
% (with some adjustment in the base case)
butlast([_],[]) :- !.
butlast([H|T], [H|L]) :- butlast(T,L). 

%
% Here we just take the divisors
% from 2..sqrt(N) and then add the rest (N mod Divisors).
% For some N the last element in Divisors1 is the same as
% first element in Divisors2 which we must handle.
% This happens when N is a square.
%
% This is faster than divisors/2.
%
divisors2(N, Divisors) :-
        N1 is N+1,
        X is integer(floor(sqrt(N1))), 

        % get the first half of the divisors
        ( for(I,1,X), 
          fromto(Divisors1,Out1,In1,[]),
          param(N) do 
              N mod I =:= 0 -> 
              Out1 = [I|In1]
        ; 
              Out1 = In1
        ),
        % now make the second half
        ( foreach(D1,Divisors1),
          foreach(D2,Divisors2),
          param(N) do
              D2 is N // D1
        ),
        % When N is a square we must remove the first element in 
        % the reversed list (since it's the same as the last in the
        % first list)
        reverse(Divisors2,Divisors2b),
        last_element(Divisors1, Last1),
        Divisors2b = [First2b|Divisors2c],
        (
            Last1 \= First2b ->
                append(Divisors1,Divisors2b,Divisors)
        ;
                append(Divisors1,Divisors2c,Divisors)
        ).   

last_element(List,Last) :-
        length(List,Len),
        nth1(Len,List,Last).
        
% Is N a square number?
is_square(N) :- 
        Y is integer(floor(sqrt(N))),
        N =:= Y*Y.
        

% Proper divisors, i.e. without N.
divisors_proper(N,D) :-
        divisors2(N,D1),
        butlast(D1,D).


amicable(N) :-
        % (What is the proper divisor of 0?)
        N > 1,
        divisors_proper(N,DA), sum(DA,A),
        divisors_proper(A,DB), sum(DB,B),
        A \= B,
        N =:= B.

% (0.24s)
problem21 :-
        % using bagof to get the distinct solutions
        bagof(N,(between(1,9999,N), amicable(N)), List), 
        writeln(List),
        sum(List,Sum),
        writeln(Sum).
  