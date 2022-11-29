/*

  Euler problem 23 in SICStus Prolog

  """
  A perfect number is a number for which the sum of its proper divisors 
  is exactly equal to the number. For example, the sum of the proper divisors 
  of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than 
  n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
  that can be written as the sum of two abundant numbers is 24. By mathematical 
  analysis, it can be shown that all integers greater than 28123 can be written 
  as the sum of two abundant numbers. However, this upper limit cannot be reduced 
  any further by analysis even though it is known that the greatest number that 
  cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of 
  two abundant numbers.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).


go :- 
        L = [
             euler23a % ,
             % euler23b,
             % euler23c
            ],
        run_problems(L).

%%
%% 6.012s
%%
euler23a :-
        %% N = 28123, 
        %% From http://mathworld.wolfram.com/AbundantNumber.html: 
        %%  "Every number greater than 20161 can be expressed as a
        %% sum of two abundant numbers."
        N = 20161,
        numlist(1,N,Is),

        %% Get the Abundant numbers
        include(abundant,Is,Abundant),

        %% Find all numbers that can be
        %% written as a sum of two Abundant numbers
        findall(AB,
                (member(A,Abundant),
                 member(B,Abundant),
                 A =< B,
                 AB is A+B,
                 AB < N
                ),
                ABs),
        %% And now delete these from 1..N
        %% ABs: 6257244 numbers (including duplicates)
        %% ABsSorted: 18705 numbers
        sort(ABs,ABsSorted),    % sort and remove duplicates
        delete_all(Is,ABsSorted,L),
        %% subtract(Is,ABsSorted,L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% All clpfd version: 54.803s
%%
euler23b :-
        %% N = 28123, 
        %% From http://mathworld.wolfram.com/AbundantNumber.html: 
        %%  "Every number greater than 20161 can be expressed as a
        %% sum of two abundant numbers."
        N = 20161,
        numlist(1,N,Is),

        %% Get the Abundant numbers
        include(abundant2,Is,Abundant),

        %% Find all numbers that can be
        %% written as a sum of two Abundant numbers
        findall(AB,
                (member(A,Abundant),
                 member(B,Abundant),
                 A #=< B,
                 AB #= A+B,
                 AB #< N
                ),
                ABs),
        %% And now delete these from 1..N
        %% ABs: 6257244 numbers (including duplicates)
        %% ABsSorted: 18705 numbers
        sort(ABs,ABsSorted),    % sort and remove duplicates
        delete_all(Is,ABsSorted,L),
        %% subtract(Is,ABsSorted,L),
        sum_list(L,Sum),
        writeln(Sum).

%%
%% Different clpfd approach. 17.165s
%%
euler23c :-
        N = 20161,
        numlist(1,N,Is),
        %% Get the Abundant numbers
        include(abundant2,Is,Abundant),
        list_domain_disjunction(Abundant,Domain),
        findall(AB,e23c(N,Domain,AB),ABs),
        sort(ABs,ABsSorted),
        delete_all(Is,ABsSorted,L),
        sum_list(L,Sum),
        writeln(Sum).


e23c(N,Domain, AB) :-
        A in Domain,
        B in Domain,
        A #=< B,
        AB #= A+B,
        AB #< N,
        labeling([ff,bisect],[A,B]).

abundant(N) :-
        sum_proper_divisors(N,D),
        D > N.


abundant2(N) :-
        sum_proper_divisors_clp(N,D),
        D #> N.


%%
%% sum_divisors2(N,Sum)
%%
%% Sum is the sum of (proper) divisors of N (including 1 but not including N).
%%
sum_proper_divisors_clp(N,Sum) :-
        sum_proper_divisors_clp(2,N,1,Sum), !.

sum_proper_divisors_clp(I,N,Sum,Sum) :-
        I > floor(sqrt(N)).

% I is a divisor of N
sum_proper_divisors_clp(I,N,Sum0,Sum) :-
        N mod I #= 0,
        NdivI #= N div I,
        Sum1 #= Sum0 + I,
        (I #\= NdivI
        -> 
         Sum2 #= Sum1 + NdivI
        ; 
         Sum2 #= Sum1
        ),
        I1 #= I+1,
        sum_proper_divisors_clp(I1,N,Sum2,Sum).

% I is no divisor of N.
sum_proper_divisors_clp(I,N,Sum0,Sum) :-
        % N mod I \= 0,
        I1 #= I+1,
        sum_proper_divisors_clp(I1,N,Sum0,Sum).
