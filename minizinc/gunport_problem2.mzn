%
% The Gunport Problem in MiniZinc;
%
% Problem and original OPL model from 
% Martin Chlond & Robert Bosch "The Gunport Problem"
% http://archive.ite.journal.informs.org/Vol6No2/ChlondBosch/index.php
%
% OPL code: 
% http://archive.ite.journal.informs.org/Vol6No2/ChlondBosch/gunport2.php
%
% Original note from the OPL mode
%  Model name   : gunport2.mod
%  Description  : solves Gunport puzzles
%  Source       : Martin Gardner 
%  Date written : 3/2/06
%  Written by   : Martin Chlond, Lancashire Business School
%  Email        : mchlond@uclan.ac.uk 

int: m = 4;
int: n = 4; 
% int: m = 5;
% int: n = 5; % 

int: m3 = m mod 3;
int: n3 = n mod 3;

set of 1..m: M = 1..m;
set of 1..n: N = 1..n;
set of 1..p: P = 1..p;

array[M,N,P] of var 0..1: x;

% compute required number of dominoes
int: p = 
if m3*n3 = 0 then
   (m*n) div 3
elseif m3==n3 then
    (m*n+2) div 3
else
  (m*n+1) div 3
endif
;

% the output matrix
array[M,N] of var 0..n*m: rt;

var int: sum_zeros = sum(i in M, j in M) (bool2int(rt[i,j] = 0));
% var int: z = sum(k in P) (d[k]);

% solve :: int_search([x[i,j,k] | i in M, j in N, k in P], first_fail, indomain_min, complete) maximize sum_zeros;
solve :: int_search([x[i,j,k] | i in M, j in N, k in P], first_fail, indomain_min, complete) satisfy;

constraint
  sum_zeros >= 4
  /\
  % each domino in adjacent cells 
  forall(i in M,j in N,k in P) (
        sum(l in j-1..j+1 where l >= 1 /\ l <= n /\ l != j) (x[i,l,k]) +
        sum(l in i-1..i+1 where l >= 1 /\ l <= m /\ l != i) (x[l,j,k]) >= x[i,j,k]
  )

  /\
  %  each domino covers two cells 
  forall(k in P) (
    sum(i in M,j in N) (x[i,j,k]) = 2
  )
  /\
  % each cell covered by, at most, one domino 
  forall(i in M,j in N) (
      sum(k in P) (x[i,j,k]) <= 1
  )
  /\
  % no two adjacent cells vacant (rows) 
  forall(i in M,j in 1..n-1) (
    sum(c in j..j+1,k in P) (x[i,c,k]) >= 1
  )
  /\
  % no two adjacent cells vacant (columns)
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
  "sum_zeros: " ++ show(sum_zeros)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(rt[i,j])
  | i in M, j in N
] ++ ["\n"];