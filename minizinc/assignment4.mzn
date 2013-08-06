% 
% Assignment in MiniZinc.
% 
% A different variant from assignment2.mzn etc.
% From http://ssdi.di.fct.unl.pt/~pb/cadeiras/cp/cp8_global1.pdf
% Page 24
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 


% A1:1..2, A2::1..2, A3::1..3, A4::2..5, A5:: 3..5,
% B1:1..3, B2::1..4, B3::3..5, B4::4..5, B5:: 4..5,
% assignment([A1,A2,A3,A4,A5], [B1,B2,B3,B4,B5])


int: n = 5;

var 1..2: A1;
var 1..2: A2;
var 1..3: A3;
var 2..5: A4;
var 3..5: A5;

var 1..3: B1;
var 1..4: B2;
var 3..5: B3;
var 4..5: B4;
var 4..5: B5;

array[1..n] of var int: A = [A1,A2,A3,A4,A5];
array[1..n] of var int: B = [B1,B2,B3,B4,B5];

array[1..n,1..2] of var 1..5: x;


solve satisfy;

constraint
  
  forall(i in 1..n) (
    x[i,1] = i  % symmetry breaking set index of A as 1..n
    /\ 
    A[x[i,1]] = B[x[i,2]]
  )
  /\
  forall(j in 1..2) (
     all_different([x[i,j] | i in 1..n])
  )
;


output 
[ 
  "\nvalue in A: ", show(A), "\n",
  "value in B: ", show(B)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j]) ++
  if j = 2 then " A:" ++ show(A[x[i,1]]) ++ " B:" ++ show(A[x[i,1]]) else " " endif 

  | i in 1..n, j in 1..2

] ++ ["\n"];