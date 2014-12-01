% 
% M queens on a NxN board in MiniZinc.
% 
% From
% 5 Queens Puzzle Algorithm and Possible Soultions
% http://stackoverflow.com/questions/27101367/5-queens-puzzle-algorithm-and-possible-soultions
% """
% How many possible solutions are there for a 5 queens puzzle on an 8x8 chessboard? 
% Would also like to have a simple algorithm and C++ code for the execution of the same.
% """

% n (board size)   m (num queens)   #solutions
%    8             1                   64
%    8             2                 1288
%    8             3                10320
%    8             4                34568
%    8             5                46736
%    8             6                22708
%    8             7                 3192
%    8             8                   92
%
%
% n (board size)   m (num queens)   #solutions
%    5             5                   10
%    6             5                  248
%    7             5                 4618
%    8             5                46736
%    9             5               310496
%   10             5              1535440
%   11             5              6110256      
%   12             5             20609544
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 8; % n x n board
int: m = 5; % m queens

% decision variables
% array[1..n] of var opt 1..m: q;
array[1..n] of var 0..n: q;

solve satisfy;
% solve :: int_search(q, first_fail, indomain_median, complete) satisfy;

constraint
   forall(i, j in 1..n where i < j) (
      (q[i] != 0 /\ q[j] != 0) 
      -> 
      (
         q[i] != q[j] /\
         q[i]+i != q[j]+j /\
         q[i]-i != q[j]-j
      )
   )

; 

constraint sum([bool2int(q[i] > 0) | i in 1..n]) = m;

% symmetry breaking
% constraint q[1] = 1;

output [ show(q)];

