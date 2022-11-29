/*

  Euler problem 36 in SICStus Prolog

  """
  The decimal number, 585 = 1001001001_(2) (binary), is palindromic 
  in both bases.
  
  Find the sum of all numbers, less than one million, which are palindromic 
  in base 10 and base 2.

  (Please note that the palindromic number, in either base, may not 
   include leading zeros.)
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler36a
            ],
        run_problems(L).

%%
%% 0.237s
%%
euler36a :-
        findall(N,
                (between(1,999999,N),
                 palindromic2(N),
                 dec_to_base_list(N,2,L2),
                 palindromic(L2)
                ),
                L),
        sum_list(L,Sum),
        writeln(Sum).
        
