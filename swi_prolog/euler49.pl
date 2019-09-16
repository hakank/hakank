/*

  Euler problem 49 in SWI Prolog

  """  
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
  increases by 3330, is unusual in two ways: (i) each of the three terms are 
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms 
  in this sequence?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler49a
            ],
        run_problems(L).

%%
%% Using clpfd: 0.10s
%%
euler49a :-
        N = 4,
        L = [A,B,C],
        L ins 1001..9999,

        A #\= 1487,
        A #< B,
        B #< C,

        B - A #= 3330,
        C - B #= 3330,  

        prime_cp(A),
        prime_cp(B),
        prime_cp(C),

        length(AL,4),
        AL ins 0..9,
        to_num(AL,A),
        
        length(BL,N),
        BL ins 0..9,
        to_num(BL,B),
        
        length(CL,N),
        CL ins 0..9,        
        to_num(CL,C),

        length(JAB,N),
        JAB ins 1..4,
        all_distinct(JAB),
        permutation_cp(AL,BL,JAB),
        
        length(JBC,N),
        JBC ins 1..4,
        all_distinct(JBC),  
        permutation_cp(BL,CL,JBC),

        flatten([L,AL,BL,CL,JAB,JBC], Vars),
        labeling([ff,bisect,down],Vars),
        maplist(num_to_digit_list,L,Ls),
        flatten(Ls,Sol1),
        digit_list_to_num(Sol1,Sol),
        writeln(Sol).
