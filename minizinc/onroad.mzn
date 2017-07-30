% 
% On the road puzzle in MiniZinc.

% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 1.
% Description  : On the road
% Source       : Poniachik, J. & L, (1998), Hard-to-solve Brainteasers, Sterling

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s1.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

set of 1..10: N = 1..10;
array[N,N] of var 0..1: x; %  x(i,j) = 1 if digit (i-1) is in position j
var int: d;
array[N] of var 0..9: y; % the number (not a proper integer programming constraint)

var int: closest =  sum(i in N) (10*(i-1)*x[i,1]) + sum(i in N) ((i-1)*x[i,2]);

% solve minimize closest;
solve :: int_search([x[i,j] | i,j in N], first_fail, indomain_max, complete) minimize closest;


constraint 
  % note: this constraint is not a proper integer programming constraint
  forall(i in N) (
     let {
       var 1..10: j
     }
     in
     x[i,j] = 1
     /\
     y[i] = j -1
  )
  /\
  forall(i in N) (
        sum(j in N) (x[i,j]) = 1
  )
  /\
  forall(j in N) (
        sum(i in N) (x[i,j]) = 1
  )
  /\
  x[1,1]+x[1,3]+x[1,5]+x[1,7]+x[1,9] = 0
  /\
  sum(i in N) (10*(i-1)*x[i,1]) + sum(i in N) ((i-1)*x[i,2]) + d   =
        sum(i in N) (10*(i-1)*x[i,3]) + sum(i in N) ((i-1)*x[i,4])
  /\
   sum(i in N) (10*(i-1)*x[i,3])  + sum(i in N) ((i-1)*x[i,4]) + d =
   sum(i in N) (10*(i-1)*x[i,5]) + sum(i in N)((i-1)*x[i,6])
  /\
  sum(i in N) (10*(i-1)*x[i,5]) + sum(i in N) ((i-1)*x[i,6]) + d =
  sum(i in N) (10*(i-1)*x[i,7]) +sum(i in N) ((i-1)*x[i,8])
  /\
  sum(i in N) (10*(i-1)*x[i,7]) + sum(i in N) ((i-1)*x[i,8]) + d =
  sum(i in N) (10*(i-1)*x[i,9]) + sum(i in N) ((i-1)*x[i,10])
;

output 
[
  "y: ", show(y), "\n",
  "d: ", show(d)
]
++
[
  if i = 1 /\ j = 1 then
   "\nclosest: " ++ show(closest) 
  else "" endif ++
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i,j in N
] ++ ["\n"];
