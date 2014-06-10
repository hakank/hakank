% 
% The Gentle Art of Stamp-licking puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 4.
% Description  : The gentle art of stamp-licking
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s4.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: size = 4;
int: stamp = 5;

set of 1..size: S = 1..size;
set of 1..stamp: T = 1..stamp;

array[S,S,T] of var 0..1: x;
array[S,S,T] of var 0..1: a;
array[S,S] of var 1..stamp: y; % the result

var int: z = sum(i in S,j in S,k in T) (k*x[i,j,k]);

solve :: int_search([x[i,j,k] | i,j in S, k in T], 
        first_fail, indomain_min, complete)  maximize z;
% solve maximize z;

constraint
  forall(i in S,j in S) (
    sum(k in T) (x[i,j,k]) <= 1
  )
  /\
  % a(i,j,k) = 1 if stamp on square (i,j) is in line with similar stamp
  forall(i in S,j in S,k in T) (
    sum(m in S where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j,k])+
    sum(m in S where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m,k])+
    sum(m in S where m != i) (x[m,j,k]) + sum(m in S where m != j) (x[i,m,k]) <= 99*a[i,j,k]
    )
  /\ % square not both occupied and attacked by same stamp value
  forall(i in S,j in S,k in T) (
        a[i,j,k]+x[i,j,k] <= 1
  )
  /\ % calculate the output matrix
  forall(i,j in S) (
    y[i,j] = sum(k in T) (k*x[i,j,k])
  )

;


output 
[ 
  "z: ", show(z)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(y[i,j])
  | i, j in S
];
