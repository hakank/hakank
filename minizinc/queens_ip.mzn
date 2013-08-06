% 
% Queens problem, integer programming in MiniZinc.
% 
% From GLPK:s example queens.mod
% """
% QUEENS, a classic combinatorial optimization problem
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
%
% The Queens Problem is to place as many queens as possible on the 8x8
% (or more generally, nxn) chess board in a way that they do not fight
% each other. This problem is probably as old as the chess game itself,
% and thus its origin is not known, but it is known that Gauss studied
% this problem.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% size of the chess board 
int: n = 8;

% x[i,j] = 1 means that a queen is placed in square [i,j] 
array[1..n,1..n] of var 0..1: x;


% objective is to place as many queens as possible 
var int: obj = sum(i,j in 1..n) (x[i,j]);
solve :: int_search(
    [x[i,j] | i,j in 1..n] ++
    [obj],
   first_fail, % "occurrence",
   indomain_min,
   complete
   )
   maximize obj;

constraint

   % at most one queen can be placed in each row 
   forall(i in 1..n) (
        sum(j in 1..n) (x[i,j]) <= 1
   )
   /\
   % at most one queen can be placed in each column 
   forall(j in 1..n) (
     sum(i in 1..n) (x[i,j]) <= 1
   )
   /\
   % at most one queen can be placed in each "\"-diagonal 
   forall(k in 2-n..n-2) (
        sum(i,j in 1..n where i-j == k) (x[i,j]) <= 1
   )
   /\
   % at most one queen can be placed in each "/"-diagonal 
   forall(k in 3..n+n-1) (
     sum(i,j in 1..n where i+j == k) (x[i,j]) <= 1
   )
;

output 
[ "\nz: ", show(obj) ] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++ ["\n"];

% and print its optimal solution 
% for {i in 1..n}
% {  for {j in 1..n} printf " %s", if x[i,j] then "Q" else ".";
%    printf("\n");
% }
