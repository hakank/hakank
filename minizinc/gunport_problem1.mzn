% 
% Gunport problem in MiniZinc.
%
% Problem and original OPL model from 
%   From Martin Chlond & Robert Bosch "The Gunport Problem"
%   http://archive.ite.journal.informs.org/Vol6No2/ChlondBosch/index.php
%   OPL code: 
%   http://archive.ite.journal.informs.org/Vol6No2/ChlondBosch/gunport1.php
%
% Note in the original model:  
% Model name   : gunport1.mod
% Description    : solves Gunport puzzles - modified
% Source          : Martin Gardner 
% Date written  : 3/2/06
% Written by     : Martin Chlond, Lancashire Business School
% Email            : mchlond@uclan.ac.uk 

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: m = 4;
int: n = 4; 
int: p = 8;

set of int: M = 1..m;
set of int: N = 1..n;
set of int: P = 1..p;

array[M,N,P] of var 0..1: x;
array[P] of var 0..1: d;

% the output matrix
array[M,N] of var 0..n*m: rt;

var int: z = sum(k in P) (d[k]);

solve :: int_search([x[i,j,k] | i in M, j in N, k in P], first_fail, indomain_min, complete) minimize z;
% solve :: int_search([x[i,j,k] | i in M, j in N, k in P], first_fail, indomain_min, complete) satisfy;

constraint

  % each domino in adjacent cells 
  forall(i in M,j in N,k in P) (
       (
        sum(l in j-1..j+1 where l >= 1 /\ l <= n /\ l != j) (x[i,l,k]) +
        sum(l in i-1..i+1 where l >= 1 /\ l <= m /\ l != i) (x[l,j,k] )
       )
      >= x[i,j,k]
  )

  /\ % each domino covers two cells 
  forall(k in P) (
     sum(i in M,j in N) (x[i,j,k]) = 2*d[k]
  )

  /\ % each cell covered by, at most, one domino
  forall(i in M,j in N) (
    sum(k in P) (x[i,j,k]) <= 1
  )

  /\ % no two adjacent cells vacant (rows)
  forall(i in M,j in 1..n-1) (
       sum(c in j..j+1,k in P) (x[i,c,k]) >= 1
  )

  /\ % no two adjacent cells vacant (columns)
  forall(i in 1..m-1,j in N) ( 
     sum(r in i..i+1,k in P) (x[r,j,k]) >= 1
  )

  /\ % for the output
  forall(i in M) (
    forall(j in N) (
      rt[i,j] = sum(k in P) (k*x[i,j,k])
    )
  )


;


output 
[
  "z: " ++ show(z)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(rt[i,j])
  | i in M, j in N
] ++ ["\n"];