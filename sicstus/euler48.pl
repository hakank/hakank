/*

  Euler problem 48 in SICStus Prolog

  """
  The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
  
  Find the last ten digits of the series, 
  1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000).
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sistus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler48a
            ],
        run_problems(L).

%%
%% 0.023s
%%
euler48a :-
        findall(J,
                (
                between(1,1000,I),
                 J is I^I
                ),
                L
               ),
        sum_list(L,Sum1),
        number_chars(Sum1,Sum2),
        length(L10,10),
        append(_,L10,Sum2),
        number_chars(Sol,L10),
        writeln(Sol).

        


