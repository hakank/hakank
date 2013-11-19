% 
% Tic tac toe avoidance in MiniZinc.
% 
% From Richard Wiseman
% http://richardwiseman.wordpress.com/2013/11/15/its-the-friday-puzzle-233/
% """
% Can you place six X’s on this noughts and crosses board without making 
% three-in-a-row in any direction?
% """
%
% This model assumes that it's not a trick question (which it very well might be).
% 
% It's no trick question: there are two different ways:
%
%   0 1 1
%   1 0 1
%   1 1 0
%   ----------
% 
%   1 1 0
%   1 0 1
%   0 1 1
% 
% Hmm, I assume that this mean that just the 'X's (1's) should don't 
% make three-in-a-row. Both these solutions give one three-in-a-row 
% for the 'O's (0's).
% 

% 
% Note:
%  There are 512 possible states of the board with arbitrarily number of 0's and 1's.
%  84 of these has 6 'X' (=1).
% 

% Here are the number of solutions for the general NxN problem with M=N*N-N 'X's 
% and no N-1 X in any directions:
%
%   N    # solutions
%   --------------
%   1        1
%   2        0
%   3        2
%   4       10
%   5       48
%   6      270
%   7     2004
%   8    15406
%   9   144656
%  10  1399070
% 

% 1,0,2,10,48,270,2004,15406,144656,1399070

% Eureqa give this formula, though with just 0.039 fit
%   (n*delay(sols,1))
% 

%  This sequence is not in OEIS:
%  http://oeis.org/search?q=1%2C0%2C2%2C10%2C48%2C270%2C2004%2C15406&sort=&language=english&go=Search

% With the symmetry breaking x[1,1] = 0
% 
%   N    # solutions
%   --------------
%   1      1
%   2      0
%   3      1
%   4      3
%   5     13
%   6     67
%   7    411
%   8   2921
%   9  23633
%  10 214551
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 3;
int: m = n*n-n;
array[1..n,1..n] of var 0..1: x;



% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], most_constrained, indomain_min, complete) satisfy;

constraint
   trace("n: " ++ show(n) ++ " m: " ++ show(m) ++ "\n", true) /\
   sum([x[i,j] | i, j in 1..n]) = m
   /\
   forall(i in 1..n) (
     % note: These can be changed to "=" without affecting the
     %       number of solutions.
     sum([x[i,j] | j in 1..n]) = n-1 /\
     sum([x[j,i] | j in 1..n]) = n-1
   )
   /\ % diagonals
   sum([x[i,i] | i in 1..n]) <= n-1 /\
   sum([x[i,n-i+1] | i in 1..n]) <= n-1

   /\ % symmetry breaking 
   x[1,1] = 0
;

output [

   if j = 1 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 1..n
];

