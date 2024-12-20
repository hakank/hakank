/*

  Euler #2 in SICStus Prolog.

  http://projecteuler.net/index.php?section=problems&id=2
  """
  Each new term in the Fibonacci sequence is generated by adding the previous two 
  terms. By starting with 1 and 2, the first 10 terms will be:

  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  Find the sum of all the even-valued terms in the sequence which do not exceed 
  four million.
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- ensure_loaded(hakank_utils).

go :-
        L = [
             % euler2a,
             % euler2b,
             euler2c
            ],
        run_problems(L),
        nl.


%%
%% 0.001s
%%
euler2a :-
        new_mutdict(Dict),
        mutdict_clear(Dict),
        gen2(Dict,Total),         % get all < 4000000
        include(even,Total,L),
        sum_list(L,Sum),
        writeln(Sum).


% This generates a list of all fib numbers < 4000000
gen2(Dict,Total) :-
        gen2(Dict,1, 1, Total).

% Some trickery to remove last element.
gen2(Dict,C, _, [F|Total]) :-
        fib3(Dict,C, F),
        (F < 4000000
        ->
         C1 is C+1,
         gen2(Dict,C1, F, Total)
        ;
         Total = []
        ).


%%
%% 0.001s
%% Using and upper bound of N is perhaps considered cheating.
%%
euler2b :-
        new_mutdict(Dict),
        mutdict_clear(Dict),
        numlist(1,100,I),       % The 100th Fib is large enough...
        maplist(fib3(Dict),I,Fibs),
        %% include(less_than_and_even(4000000),Fibs,L),
        include(>(4000000),Fibs,L1),
        include(even,L1,L),
        sum_list(L,Sum),
        writeln(Sum).

%% less_than_and_even(Y,X) :- X < Y, even(X). % Not used

%%
%% 0.001s
%% Using accumulator
%%
euler2c :-
        new_mutdict(Dict),
        e2c(Dict,1,4000000,0,0,S),
        writeln(S).

e2c(_Dict,_N,Limit,F, S,S) :-
        F >=  Limit.
e2c(Dict,N,Limit,F,S0,S) :-
        F < Limit,        
        fib3(Dict,N,F2),
        F2 mod 2 =:= 0,
        N1 is N+1,
        S0F is S0+F,
        e2c(Dict,N1,Limit,F2, S0F,S).
e2c(Dict,N,Limit,F,S0,S) :-
        N1 is N+1,
        e2c(Dict,N1,Limit,F,S0,S).

