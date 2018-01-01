% 
% 3-in-a-row puzzle in MiniZinc.
% 
% From https://stackoverflow.com/questions/36202351/3-in-a-row-logic-puzzle-optimisation-for-sequence-constraints-in-lists-arrays
% """
% In the following puzzle we try and fill the grid with blue and white squares in such a way that:
%
%     A 3-in-a-row (or column) of the same colour is not allowed.
%     Each row and column has an equal number of blue and white squares.
% 
% [example_puzzle: (W: White, B: Blue)
%     W _ _ _ B _
%     _ W _ _ _ _
%     _ _ _ _ _ W
%     B _ _ W _ _
%     _ _ B B _ _
%     _ W _ _ B _
%]
%
% If we now represent white with a 0 and blue with a 1, we get:
%
% 0 _ _ _ 1 _
% _ 0 _ _ _ _
% _ _ _ _ _ 0
% 1 _ _ 0 _ _
% _ _ 1 1 _ _
% _ 0 _ _ 1 _
%
% And we can pretty quickly verify that
%
% 0 1 0 0 1 1 
% 0 0 1 1 0 1 
% 1 1 0 1 0 0 
% 1 1 0 0 1 0 
% 0 0 1 1 0 1 
% 1 0 1 0 1 0 
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: n2 = n div 2;

% decision variables
array[1..n, 1..n] of var 0..1: x;


% solve satisfy;
solve :: int_search(array1d(x), occurrence, indomain_split, complete) satisfy;

constraint
  forall(i in 1..n) (
    % same number of white and blue
    sum([x[i,j] | j in 1..n]) = n2 /\
    sum([x[j,i] | j in 1..n]) = n2 /\
    % not 3 in a row
    sliding_sum(1,2,3,[x[i,j]=0 | j in 1..n]) /\
    sliding_sum(1,2,3,[x[i,j]=1 | j in 1..n]) /\
    sliding_sum(1,2,3,[x[j,i]=0 | j in 1..n]) /\
    sliding_sum(1,2,3,[x[j,i]=1 | j in 1..n]) 
  )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n
];

% Problem 1:
% problem(p(6,1),[(1,1,0),(1,5,1),(2,2,0),(3,6,0),(4,1,1),(4,4,0),(5,3,1),(5,4,1),(6,2,0),(6,5,1)]).
% n = 6;
% x = array2d(1..n,1..n,
% [
% 0,_,_,_,1,_,
% _,0,_,_,_,_,
% _,_,_,_,_,0,
% 1,_,_,0,_,_,
% _,_,1,1,_,_,
% _,0,_,_,1,_,
% ]);

% Problem 2:
% problem(p(18,1),[(1,3,0),(1,9,0),(1,10,0),(1,12,0),(1,14,0),(1,18,1),(2,4,0),(2,7,1),(2,8,1),(3,2,1),(3,6,0),(3,16,0),(3,17,0),(4,2,1),(4,4,1),(4,10,1),(4,13,1),(4,18,1),(5,8,1),(5,10,1),(5,15,0),(5,16,1),(6,12,1),(7,3,0),(7,4,0),(7,6,1),(7,9,0),(7,12,1),(7,17,0),(8,1,1),(8,4,0),(8,8,1),(8,15,1),(8,16,1),(9,7,0),(9,10,0),(9,14,0),(10,2,1),(10,4,1),(10,6,1),(10,13,1),(11,7,0),(11,10,1),(12,1,1),(12,4,1),(12,7,1),(12,15,1),(12,16,1),(13,1,1),(13,6,0),(13,8,1),(13,10,0),(13,16,1),(14,5,1),(14,10,0),(14,13,1),(15,1,1),(15,3,1),(15,12,0),(15,13,1),(15,15,0),(16,2,1),(16,4,0),(16,12,0),(16,18,0),(17,9,0),(17,15,0),(17,18,0),(18,2,1),(18,8,1),(18,11,1),(18,15,1),(18,16,1)]).
n = 18;
x = array2d(1..n,1..n,
[
_,_,0,_,_,_,_,_,0,0,_,0,_,0,_,_,_,1,
_,_,_,0,_,_,1,1,_,_,_,_,_,_,_,_,_,_,
_,1,_,_,_,0,_,_,_,_,_,_,_,_,_,0,0,_,
_,1,_,1,_,_,_,_,_,1,_,_,1,_,_,_,_,1,
_,_,_,_,_,_,_,1,_,1,_,_,_,_,0,1,_,_,
_,_,_,_,_,_,_,_,_,_,_,1,_,_,_,_,_,_,
_,_,0,0,_,1,_,_,0,_,_,1,_,_,_,_,0,_,
1,_,_,0,_,_,_,1,_,_,_,_,_,_,1,1,_,_,
_,_,_,_,_,_,0,_,_,0,_,_,_,0,_,_,_,_,
_,1,_,1,_,1,_,_,_,_,_,_,1,_,_,_,_,_,
_,_,_,_,_,_,0,_,_,1,_,_,_,_,_,_,_,_,
1,_,_,1,_,_,1,_,_,_,_,_,_,_,1,1,_,_,
1,_,_,_,_,0,_,1,_,0,_,_,_,_,_,1,_,_,
_,_,_,_,1,_,_,_,_,0,_,_,1,_,_,_,_,_,
1,_,1,_,_,_,_,_,_,_,_,0,1,_,0,_,_,_,
_,1,_,0,_,_,_,_,_,_,_,0,_,_,_,_,_,0,
_,_,_,_,_,_,_,_,0,_,_,_,_,_,0,_,_,0,
_,1,_,_,_,_,_,1,_,_,1,_,_,_,1,1,_,_,
]);