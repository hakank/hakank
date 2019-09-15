/*

  Euler problem 24 in SWI Prolog

  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
   
      012   021   102   120   201   210
  
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """ 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             %% euler24a,
             euler24b
             %% , euler24c
            ],
        run_problems(L).

%%
%% 3.4s
%%
euler24a :-
        set_prolog_stack(global, limit(2_000_000_000)),
        numlist(0,9,L),
        findall(P,permutation(L,P),Permutations),
        nth1(1_000_000,Permutations,SolList),
        digit_list_to_num(SolList,Sol),
        writeln(Sol).

%%
%% 2.8s
%%
euler24b :-
        set_prolog_stack(global, limit(10_000_000_000)),
        N = 1_000_000,
        numlist(0,9,L),
        find_permutation(1,N,L,P),
        digit_list_to_num(P,Sol),
        writeln(Sol).

%%
%% Too slow!
%% 
euler24c :-
        set_prolog_stack(global, limit(2_000_000_000)),
        findall(L,permutation_clpfd(L,10),Permutations),
        nth1(1_000_000,Permutations,SolList),
        digit_list_to_num(SolList,Sol),
        writeln(Sol).


find_permutation(N,N,P,P).
find_permutation(I,N,P0,P) :-
        next_higher_permutation(P0,P1),
        I1 is I+1,
        find_permutation(I1,N,P1,P).

%%
%% Too slow for this
%%
permutation_clpfd(L, N) :-
        length(L, N),
        N1 #= N - 1,
        L ins 0..N1,
        all_different(L),
        labeling([bisect],L).

%%
%% next_higher_permutation/2
%%
%% From T. Van Le, "Techniques of Prolog Programming", page 100f
%% 
next_higher_permutation(L,L1) :-
   reverse3(L,[],L2),
   append(A,[X,Y|B],L2), X > Y,
   append(A,[X],C),
   append(A1,[U|B1],C), U > Y,
   append(A1,[Y|B1], B2),
   reverse3([U|B], B2,L1).

%
% reverse3/3
%
% From T. Van Le, "Techniques of Prolog Programming", page 99
reverse3([],R,R).
reverse3([H|T],R,L1) :-
        reverse3(T,[H|R],L1).

