% 
% Takuzu grid puzzle in MiniZinc.
% 
% From http://en.wikipedia.org/wiki/Takuzu
% """
% Takuzu is a logic-based number placement puzzle. The objective is to fill a 
% (usually 10x10) grid with 1s and 0s, where there is an equal number of 1s and 
% 0s in each row and column and no more than two of either number adjacent to 
% each other. Additionally, there can be no identical rows or columns. 
% """

% Compare with 
%    http://www.hakank.org/minizinc/binero.mzn 
% for another approach
% 
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: m = n div 2;
array[1..n, 1..n] of var 0..1: problem;

% decision variables
array[1..n, 1..n] of var 0..1: x;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint
  % initialize the problem
  forall(i,j in 1..n) (
     x[i,j] = problem[i,j]
  )

  /\ % equal number of 0s and 1s in each row and column
  forall(i in 1..n) (
     sum([bool2int(x[i,j] = 0) | j in 1..n]) = m
     /\
     sum([bool2int(x[j,i] = 0) | j in 1..n]) = m
  )

  /\ % no more than two of the same values adjacent
  forall(i in 2..n-1) (
       forall(j in 1..n) (
          (x[i-1,j] != x[i,j] \/ x[i,j] != x[i+1,j]) 
          /\
          (x[j, i-1] != x[j,i] \/ x[j,i] != x[j,i+1]) 
       )
  )

  /\ % no identical row or column
  forall(i,j in 1..n where i < j) (
    exists(k in 1..n) ( x[i,k] != x[j,k] )
    /\
    exists(k in 1..n) ( x[k,i] != x[k, j] )
  )
;

output [
   if j = 1 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 1..n
];

%
% data
%
% n = 4;
% problem = array2d(1..n, 1..n,
%      [_,1,_,0,
%       _,_,0,_,
%       _,0,_,_,
%       1,1,_,0]);

%
% Problem instance from the Scampi data/binero1.txt
%
% n = 10;
% problem = array2d(1..n, 1..n,
% [
% 0,_,1,1,_,_,_,_,_,1,
% _,_,1,_,_,_,_,0,_,0,
% _,0,_,_,1,_,1,_,1,_,
% _,1,_,_,_,_,_,_,_,_,
% 1,_,0,_,_,1,_,_,_,_,
% _,0,_,_,0,_,_,_,1,1,
% _,_,_,1,_,_,_,_,_,1,
% 0,_,_,1,_,1,_,0,_,_,
% _,_,0,_,_,_,_,_,_,_,
% 1,1,_,_,_,_,1,1,_,0
% ]);

%
% Problem instance from the Scampi problem/binero2.txt
%
% n = 10;
% problem = array2d(1..n, 1..n,
% [
% _,0,_,0,_,_,_,1,_,_,
% 1,_,_,_,1,_,_,_,_,_,
% _,0,_,0,_,_,_,_,_,_,
% _,_,1,_,_,1,_,_,0,_,
% _,_,_,0,_,_,_,1,_,_,
% 1,_,_,0,_,0,_,_,_,_,
% _,_,1,_,_,_,_,0,_,0,
% _,_,0,1,_,_,_,_,1,_,
% 1,_,_,_,_,_,0,_,_,_,
% 1,_,0,_,_,_,_,_,_,1
% ]);


%
% Problem from http://www.cross-plus-a.com/puzzles.htm#Binairo
%
n = 10;
problem = array2d(1..n, 1..n,
[
0,_,_,_,_,_,_,_,_,_,
_,_,1,_,_,_,1,1,_,_,
_,1,_,_,_,_,_,_,0,_,
_,_,_,0,_,0,_,_,_,_,
_,_,_,_,_,_,_,_,_,_,
0,_,_,_,_,_,_,1,_,0,
_,1,_,1,_,_,_,_,_,_,
0,_,_,_,0,0,_,1,_,_,
1,_,_,_,_,_,_,_,_,_,
_,1,_,1,1,_,_,_,_,0,
]);
