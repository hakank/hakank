% 
% Wine cask puzzle in MiniZinc.
% 
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html
% Description  : Wine cask puzzle
% Source       : M Kraitchik - Mathematical Recreations (p 31)


%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s7.html

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: nephew   = 5;
int: casktype = 5;

set of int: N = 1..nephew;
set of int: C = 1.. casktype;
array[C] of 0.0..1.0: howfull;
array[C] of 1..10000: cdum;

array[N,C] of var 1..10: x;
array[N] of var 0..100000: dtot;

howfull = [    0.0,    0.25,  0.5,  0.75, 1.0]; 
cdum    = [10000, 1000, 100, 10,  1];



solve satisfy;
% solve :: int_search([ x[i,j] | i in N, j in C], "input_order", "indomain", "complete")  satisfy;


constraint
forall(i in N) (
    sum(j in C) (x[i,j]) = 9
)
/\
forall(i in N) (
    sum(j in C) (howfull[j]*int2float(x[i,j])) = 4.5
)
/\
forall(j in C) (
    sum(i in N) (x[i,j]) = 9
)
/\
forall(i in N) (
    sum(j in C) (cdum[j]*x[i,j]) = dtot[i]
)
/\
forall(i in 2..nephew) (
    dtot[i-1] - dtot[i] >= 1
)
;


output 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in N, j in C
]
++
["\ndtot: ", show(dtot), "\n"]
;