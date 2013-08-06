% 
% Magic square integer programming version in MiniZinc.
% 
% From GLPK:s example magic.mod
% """
% MAGIC, Magic Square
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru>
%
% In recreational mathematics, a magic square of order n is an
% arrangement of n^2 numbers, usually distinct integers, in a square,
% such that n numbers in all rows, all columns, and both diagonals sum
% to the same constant. A normal magic square contains the integers
% from 1 to n^2.
%
% (From Wikipedia, the free encyclopedia.)
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% square order
int: n = 3; 

% integers to be placed
set of 1..n*n: N = 1..n*n;

% x[i,j,k] = 1 means that cell (i,j) contains integer k
array[1..n, 1..n, N] of var 0..1: x;

array[1..n, 1..n] of var 1..n*n: square;

var int: s; % the magic sum

solve :: int_search(
        [x[i,j,k] | i,j in 1..n, k in N] ++ 
        [square[i,j] | i,j in 1..n] ++
        [s],
        first_fail,
        indomain_min, 
        complete % "credit(640, bbs(5))" % "complete"
      ) 
    satisfy;

constraint 
  s >= 0 
  /\
  s <= n*n*n
  /\
  % each cell must be assigned exactly one integer
  forall(i in 1..n, j in 1..n) (
     sum(k in N) (x[i,j,k]) = 1
  )
  /\
  % each integer must be assigned exactly to one cell
  forall(k in N) (
     sum(i in 1..n, j in 1..n) (x[i,j,k]) = 1
  )

  /\
  % the sum in each row must be the magic sum 
  forall(i in 1..n) (
     sum(j in 1..n, k in N) (k * x[i,j,k]) = s
  )

  /\
  % the sum in each column must be the magic sum
  forall(j in 1..n) (
     sum(i in 1..n, k in N) (k * x[i,j,k]) = s
  )

  /\
  % the sum in the diagonal must be the magic sum
  sum(i in 1..n, k in N) (k * x[i,i,k]) = s

  /\
  % the sum in the co-diagonal must be the magic sum
  sum(i in 1..n, k in N) (k * x[i,n-i+1,k]) = s

  /\
  % for output
  forall(i,j in 1..n) ( square[i,j] = sum(k in N) (k * x[i,j,k]))
;


output [ 
   "\ns: ", show(s)
] ++
[
  if  j = 1 then "\n" else " " endif ++
    show(square[i,j]) 
  | i,j in 1..n

] ++ ["\n"];

% printf "\n";
% printf "Magic sum is %d\n", s;
% printf "\n";
% for{i in 1..n}
% {  printf{j in 1..n} "%3d", sum{k in N} k * x[i,j,k];
%    printf "\n";
% }
% printf "\n";
