/*

  Euler problem 32 in SICStus Prolog

  """
  We shall say that an n-digit number is pandigital if it makes use of 
  all the digits 1 to n exactly once; for example, the 5-digit number, 
  15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
  containing multiplicand, multiplier, and product is 1 through 9 
  pandigital.

  Find the sum of all products whose multiplicand/multiplier/product 
  identity can be written as a 1 through 9 pandigital.
  HINT: Some products can be obtained in more than one way so be sure 
  to only include it once in your sum.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :- 
        L = [
             euler32a
            ],
        run_problems(L).

%%
%% 0.009s
%%
euler32a :-
        findall(P,pandigital(P),L),
        sort(L,Ls),
        sum_list(Ls,Sum),
        writeln(Sum).


%%
%% clpfd approach: Find a proper pandigial number
%%
pandigital(Res) :-

  % the different lengths
  Len1 in 1..2,
  Len2 in 3..4,
  Len3 #= 4,

  %% must be a 9 digit number
  Len1 + Len2 + Len3 #= 9,

  indomain(Len1), %% must be instantiated
  
  length(X1,Len1),
  domain(X1,1,9),

  length(X2,Len2),
  domain(X2,1,9),

  length(X3,Len3), % the result
  domain(X3,1,9),

  append([X1,X2,X3], Vars),
  all_different(Vars),

  %% convert to number
  toNum(X1, Num1),
  toNum(X2, Num2),
  toNum(X3, Res),

  % calculate result
  Num1 * Num2 #= Res,

  labeling([],Vars).

