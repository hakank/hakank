/*
  Euler Problem 14 in B-Prolog

  """
  The following iterative sequence is defined for the set of positive integers:
  
    n n/2 (n is even)
    n 3n + 1 (n is odd)
   
  Using the rule above and starting with 13, we generate the following 
  sequence:
   13 40 20 10 5 16 8 4 2 1
   
  It can be seen that this sequence (starting at 13 and finishing at 1) 
  contains 10 terms. Although it has not been proved yet (Collatz
  Problem), it is thought that all starting numbers finish at 1.
  
  Which starting number, under one million, produces the longest chain?
  
  NOTE: Once the chain starts the terms are allowed to go above one million.")
  """

  Answer: [n=837799,len=525]

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        time(euler14).

%%
%% From
%% http://hakank.org/picat/euler14.pi
%% which is slightly adjusted from
%% http://picat-lang.org/euler/p14.pi
%% 
%% All numbers 2..999999: 1.4s
%% Odd numbers: 3..2..999999: 1.3s
%% (My Picat version using odd numbers takes ~ 1.2s)
%%
euler14 :-
    max_chain(N,_Chain,Len),
    writeln([n=N,len=Len]).

:- table max_chain(-,-,max).
max_chain(N,Chain,Len) :-
        between(2,999999,N),  % checking all numbers        
        % between(3,2,999999,N),  % just checking the odd numbers
        gen(N,Chain,Len).

:- table gen(+,-,-).
gen(1,Chain,Len) :-
        Chain=[1],
        Len is 1.

gen(N,Chain,Len)  :-
        N > 1,
        N mod 2 =:= 0,
        T is N div 2,
        gen(T,Chain1,Len1),
        Chain=[N|Chain1],
        Len is Len1+1.

gen(N,Chain,Len) :-
        N > 1,
        N mod 2 =:= 1,
        T is 3*N+1,
        gen(T,Chain1,Len1),
        Chain=[N|Chain1],
        Len is Len1+1.
    
between(From,Step,To,N) :-
        To2 is To div Step,
        between(From,To2,Tmp),
        N is (Tmp-From)*Step+From.
