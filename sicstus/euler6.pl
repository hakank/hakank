/*

  Euler Problem 6 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=6
  """
  The sum of the squares of the first ten natural numbers is,
  12 + 22 + ... + 102 = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)2 = 552 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.
  """
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :-
        L = [
            euler6a % ,
            % euler6b,
            % euler6c
            ],
        run_problems(L).

% 0.000s
euler6a :-
        numlist(1,100,List),
        maplist(sum_sq,List,ListS),
        sum(ListS,#=,SumSquares),
        
        sum_list(List, Sum), 
        SquaresSum #= Sum*Sum, % These does not work: Sum^2, Sum**2, pow(Sum,2)
        Diff #= SquaresSum - SumSquares,
        writeln(Diff).

sum_sq(S,Sq) :- Sq #= S*S.

% 0.000s
euler6b :-
    (for(I,1,100), 
     foreach(J,List), 
     foreach(K,List2) 
     do 
     J is I^2, 
     K is I
    ), 
    sumlist(List, SumSquares), 
    sumlist(List2, Sum), 
    SquaresSum is Sum^2, 
    Diff is SquaresSum - SumSquares,
    writeln(Diff).

% 0.002s
euler6c :-
  e6c(S), 
  writeln(S).

e6c(Sum) :-
    e6c(1,0,0,Sum1),
    Sum is integer(Sum1).

e6c(N,S,T,Sum) :-
   N > 100,
   Sum is T^2 - S.

e6c(N,S,T,Sum) :- 
  S2 is S + N^2,
  T2 is T + N,
  e6c(N+1,S2,T2,Sum).
