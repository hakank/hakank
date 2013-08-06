% 
% Latin square card puzzle in MiniZinc.
% 
% Problem from Mario Livio's book about group theory
% "The Equation that couldn't be solved",
% page 22
% """
% "... Incidentally, you may get a kick out of solving this
% eighteenth century card puzzle: Arrange all the jacks,
% queens, kings, and aces from a deck of cards in a square so that 
% no suit or value would appear twice in any row, column, or the
% two main diagonals.
% """
% 
% Also see
% - http://en.wikipedia.org/wiki/Graeco-Latin_square
% - http://en.wikipedia.org/wiki/Thirty-six_officers_problem

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
int: m = 10;

% % for n=4 (and m = 10)
% set of int: cards = 
%  % values: i mod 10 
%        { 0, 1, 2, 3,  % suite 1: 0 div 10
%         10,11,12,13,  % suite 2: 1 div 10
%         20,21,22,23,  % suite 3: 2 div 10
%         30,31,32,33   % suite 4: 3 div 10
%     };

set of int: cards = { i + m*j | i,j in 0..n-1 };
        
array[1..n,1..n] of var cards: x;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_max, complete) satisfy;

constraint

   % all values must be different 
   all_different([x[i,j] | i,j in 1..n]) /\ 

   % diagonals1
   all_different([x[i,i] div m | i in 1..n ]) /\
   all_different([x[i,i] mod m | i in 1..n ]) /\

   % diagonal2
   all_different([x[i,n-i+1] div m | i in 1..n ]) /\
   all_different([x[i,n-i+1] mod m | i in 1..n ]) /\


   % rows, columns, 
   forall(i in 1..n) (
      all_different([x[i,j] div m | j in 1..n ]) /\
      all_different([x[j,i] div m | j in 1..n ]) /\

      all_different([x[i,j] mod m | j in 1..n ]) /\
      all_different([x[j,i] mod m | j in 1..n ])

   )

  /\ % symmetry breaking
  x[1,1] = 0
;


output 
[ show(cards) ++ "\n"] ++ 
[
  if j == 1 then "\n" else " " endif ++
    show(x[i,j] div m) ++ " of " ++ show(x[i,j] mod m) ++ " "
  | i,j in 1..n
] ++ ["\n"]
++
[ show(x) ++ "\n"
]
;