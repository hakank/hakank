% 
% Cihan Altay's Touching Numbers puzzle in MiniZinc.
%
% Problem and XPress Mosel model from 
% Martin J.Chlond "The Traveling Space Telescope Problem"
% http://archive.ite.journal.informs.org/Vol3No1/Chlond/index.php
% 
% XPress Mosel model:
% http://archive.ite.journal.informs.org/Vol3No1/Chlond/altayxp.html
% 
% More info about the puzzle:
% http://www.mathpuzzle.com/0123456789.htm


% Note: This model implements the formulation which are in the paper,
%       i.e. where the arrays is from 0..9 instead of 1..100
%
% This MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% mip:
% 320
% [0, 9, 8, 3, 4, 2, 7, 1, 6, 5]
%
% 0 0 0 0 0 0 0 1 0 0
% 0 0 0 0 0 0 0 0 0 0
% 0 1 0 0 0 0 0 0 0 0
% 0 0 0 0 1 0 0 0 0 0
% 0 0 0 0 0 0 0 0 0 1
% 0 0 0 1 0 0 0 0 0 0
% 0 0 1 0 0 0 0 0 0 0
% 0 0 0 0 0 1 0 0 0 0
% 0 0 0 0 0 0 1 0 0 0
% 0 0 0 0 0 0 0 0 1 0



int: Size = 9;

% N(i,j) = number of squares touching if digit (i-1) is to
%          the immediate left of digit (j-1)
array[0..Size, 0..Size] of int: N = array2d(0..Size, 0..Size, 
[0,2,4,3,3,4,5,2,5,4,
 1,0,1,1,0,1,1,0,1,1,
 4,2,0,3,3,4,4,2,4,4,
 5,2,4,0,3,4,5,2,5,4,
 5,2,4,3,0,4,5,2,5,4,
 4,1,4,3,2,0,4,1,4,3,
 4,1,4,3,2,3,0,1,4,3,
 5,2,4,3,3,4,5,0,5,4,
 5,2,4,3,3,4,5,2,0,4,
 5,2,4,3,3,4,5,2,5,0]);
            

% X(i,j) = 1 if digit (i-1) is to the immediate left of digit (j-1)
%          0 otherwise
array[0..Size, 0..Size] of var 0..1: X;
array[0..Size] of var 0..Size: Z; % variables to avoid circular sets 

% Maximize score
var int: Maxscore = sum(i, j in 0..Size) ((i+j-1)*N[i,j]*X[i,j]);

solve :: int_search([X[i,j] | i,j in 0..Size], 
        first_fail, indomain_max, complete) maximize Maxscore;
% solve maximize Maxscore;


constraint
  sum(Z) >= 1
  /\

  % Each digit to the immediate left of no more than one other
  forall(i in 0..Size) (
    sum(j in 0..Size) (X[i,j]) <= 1 
  )

  /\  % Each digit to the immediate right of no more than one other
  forall(j in 0..Size) ( 
     sum(i in 0..Size) (X[i,j]) <= 1
  )
  
  /\                                                             
  % No circular sets
  forall(i in 0..Size, j in 0..Size) (
      Z[i] - Z[j] + (Size+1)*X[i,j] <= Size
  )
  
  /\ % Exactly nine adjacencies required
  sum(i, j in 0..Size) (X[i,j]) = 9

;

output [
  show(Maxscore), "\n",
  show(Z), "\n",
] ++ [
  if j = 0 then "\n" else " " endif ++
    show(X[i,j])
  | i, j in 0..Size
];
