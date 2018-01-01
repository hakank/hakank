% 
% Tents puzzle in MiniZinc.
% 
% See for example
% http://www.crauswords.com/tents.html
% """
% A Tents puzzle consists of a square or rectangular array of squares in 
% which some of the squares contain a tree, with the remainder containing 
% only grass. The puzzle is solved by placing tents into selected grassy 
% squares so that each tree has its own tent (either horizontally or vertically), 
% and each tent has its own tree (either horizontally or vertically). 
% The squares in which tents are placed must not touch on either the sides 
% or the corners. Numbers located to the side and to the bottom of the 
% puzzle tell you how many tents are in each row and column.
%
%   Problem: T: Tree
%    _ T _ _ _ _   1
%    _ _ _ _ T _   2
%    _ _ T _ _ _   0
%    _ T _ T _ _   3
%    T _ _ _ T _   0
%    _ _ T _ _ _   2
%    
%    2 1 2 0 2 1
% 
% Solution (e: tEnt)
%    e T _ _ _ _   1
%    _ _ e _ T e   2
%    _ _ T _ _ _   0
%    e T e T e _   3
%    T _ _ _ T _   0
%    _ e T _ e _   2
%    
%    2 1 2 0 2 1
%
% """

% Also see: 
%  - https://www.brainbashers.com/tents.asp
%  - https://www.brainbashers.com/tentshelp.asp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; % num rows and columns

int: T = 1; % Tree
int: E = 2; % tEnt

array[1..n,1..n] of 0..2: puzzle;
array[1..n] of int: rows;
array[1..n] of int: cols;

% decision variables
array[1..n,1..n] of var 0..E: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i,j in 1..n) (
     if puzzle[i,j] = T then 
        x[i,j] = T /\
        % each tree has its tent
        exists(a,b in -1..1 where a+i in 1..n /\ b+j in 1..n /\ abs(a+b)=1 ) (
           x[i+a,j+b] = E
        )
     else 
       x[i,j] != T
     endif 
     /\ % and each tent has its tree
     if x[i,j] = E then 
        exists(a,b in -1..1 where a+i in 1..n /\ b+j in 1..n /\ abs(a+b)=1 ) (
           x[i+a,j+b] = T
        )
        /\ % no two tents must touch 
        not(exists(a,b in -1..1 where a+i in 1..n /\ b+j in 1..n /\ not(a = 0 /\ b=0) ) (
           x[i+a,j+b] = E
        ))
     else 
       true
     endif 
  )
  /\ % number of Trees == number of Tents
  sum([x[i,j] = E | i,j in 1..n]) = sum([x[i,j] = T | i,j in 1..n])
  /\
  forall(i in 1..n) (
     sum([x[i,j] = E | j in 1..n]) = rows[i] /\
     sum([x[j,i] = E | j in 1..n]) = cols[i] 
  )
;

output 
[
 if j = 1 then "\n" else " " endif ++
   if fix(x[i,j]) = T then "T" elseif fix(x[i,j]) = E then "e" else "_" endif
| i,j in 1..n
];


% data

% From http://www.crauswords.com/tents.html
% n = 6;
% puzzle = array2d(1..n,1..n,
% [
% 0, T, 0, 0, 0, 0,
% 0, 0, 0, 0, T, 0,
% 0, 0, T, 0, 0, 0,
% 0, T, 0, T, 0, 0,
% T, 0, 0, 0, T, 0,
% 0, 0, T, 0, 0, 0,
% ]);

% rows = [1,2,0,3,0,2];
% cols = [2,1,2,0,2,1];


% From https://www.brainbashers.com/showtents.asp?date=1231&diff=Medium&size=16
n = 16;
puzzle = array2d(1..n,1..n,
[
% 6  2  2  3  1  5  2  4  1  6  1  5  2  4  2  5
  0, T, 0, 0, 0, 0, T, 0, 0, 0, 0, 0, 0, 0, T, 0, % 7
  0, 0, 0, 0, 0, 0, 0, T, 0, T, 0, T, 0, T, T, 0, % 0
  0, 0, T, 0, 0, T, 0, T, 0, 0, 0, 0, T, 0, 0, 0, % 4
  0, T, 0, 0, 0, 0, 0, 0, 0, T, 0, 0, 0, 0, 0, 0, % 3
  0, 0, 0, T, 0, 0, 0, 0, 0, T, 0, 0, 0, 0, T, 0, % 2
  T, 0, 0, 0, 0, 0, T, 0, T, 0, 0, 0, T, 0, 0, 0, % 5
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, T, 0, 0, % 1
  T, 0, 0, 0, 0, 0, 0, 0, 0, 0, T, 0, 0, 0, T, 0, % 3
  0, 0, 0, T, 0, 0, 0, 0, T, 0, 0, 0, 0, 0, 0, 0, % 4
  T, 0, 0, T, 0, 0, T, 0, 0, 0, 0, 0, 0, 0, 0, 0, % 2 
  T, 0, 0, 0, 0, 0, 0, 0, 0, T, 0, 0, 0, T, 0, 0, % 4
  T, 0, 0, 0, 0, T, T, 0, 0, 0, 0, T, 0, 0, 0, T, % 1
  0, 0, 0, 0, 0, 0, 0, 0, 0, T, 0, T, 0, 0, 0, T, % 5
  0, 0, 0, T, 0, 0, 0, T, 0, 0, 0, 0, 0, 0, 0, 0, % 2
  T, 0, 0, 0, 0, 0, T, 0, 0, T, 0, 0, 0, 0, 0, T, % 3
  0, 0, 0, T, 0, T, 0, 0, 0, 0, T, 0, T, 0, 0, 0, % 5
]);

rows = [7,0,4,3,2,5,1,3,4,2,4,1,5,2,3,5];
cols = [6,2,2,3,1,5,2,4,1,6,1,5,2,4,2,5];
