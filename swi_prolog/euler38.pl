/*

  Euler problem 38 in SWI Prolog

  """
  Take the number 192 and multiply it by each of 1, 2, and 3:

      192 × 1 = 192
      192 × 2 = 384
      192 × 3 = 576

  By concatenating each product we get the 1 to 9 pandigital, 
  192384576. We will call 192384576 the concatenated product of 192 
  and (1,2,3)

  The same can be achieved by starting with 9 and multiplying by 
  1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the 
  concatenated product of 9 and (1,2,3,4,5).

  What is the largest 1 to 9 pandigital 9-digit number that can be 
  formed as the concatenated product of an integer with 
  (1,2, ... , n) where n > 1?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler38a
            ],
        run_problems(L).

%%
%% 0.09s
%%
euler38a :-
        findall(SS,
                (between(9,9876,N),
                 num_to_digit_list(N,S1),
                 findall(NN,
                         (between(2,3,I),
                          NI is N*I,
                          num_to_digit_list(NI,S2),
                          append(S1,S2,S),
                          length(S,9),
                          all_different(S),
                          \+ member(0,S),
                          digit_list_to_num(S,NN)
                         ),
                         SS)
                ),L),
        sort(L,Ls),
        flatten(Ls,Lf),        
        delete(Lf,[],Lf2),
        max_list(Lf2,Max),
        writeln(Max).
