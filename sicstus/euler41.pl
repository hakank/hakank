/*

  Euler problem 41 in SICStus Prolog

  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             % euler41a,
            % euler41b
            euler41c
            ],
        run_problems(L).

%%
%% 14.847s
%%
euler41a :-
        findall(P,
                (
                 between(2,9,N),
                 numlist(1,N,L),
                 permutation(L,Perm),
                 digit_list_to_num(Perm,P),
                 is_prime(P)
                ),
                L),
        maxlist(L,Max),
        writeln(Max).
        
%%
%% 14.883s
%%
euler41b :-
        
        findall(P,
                (
                 between(2,9,N),
                 numlist(1,N,L),
                 permutation(L,Perm),
                 digit_list_to_num(Perm,P),
                 is_prime3(P),
                 bb_put(p,P)
                ),
                L),
        bb_get(p,Max),
        writeln(Max).
        
%%
%% Generate all permutations in reverse order (largest to smallest)
%% 0.545s
%%
euler41c :-
    member(N,[9,8,7,6,5,4,3,2]),
    length(X,N),
    domain(X,1,N),
    all_different(X),
    toNum(X,P),    
    labeling([ff,enum,down],X),
    is_prime(P),
    writeln(P).


%%
%% Extremly slow (what a surprise! :-)
%%
euler41xxx :-
        findall(P,
                (between(3,2,987654321,P),
                 num_to_digit_list(P,Ps),
                 all_different(Ps),
                 is_prime(P)
                ),
                L),
        length(L,Len),
        writeln(Len),
        max_list(L,Max),
        writeln(Max).
