% 
% Joshua and his rats puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 12.
% Description  : Joshua and his rats
% Source       : Sole, T., (1988), The Ticket to Heaven, Penguin Books

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s12.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
int: rat = 9;
set of 1..rat: R = 1..rat;
array[R] of var 1..20: x;
array[R,R,R] of var 0..1: d;

var int: tot = sum(i in R) (x[i]);

solve :: int_search(x, first_fail, indomain_min, complete) minimize tot;
% solve minimize tot;

constraint 
  forall(i in 2..rat) (
    x[i] >= x[i-1]+1
  )
  /\
  forall(i in R,j in R,k in R where j < i /\ k < j) (
    -x[i]+2*x[j]-x[k] + 19*d[i,j,k] >= 1
  )
  /\
  forall(i in R,j in R,k in R where j < i /\ k < j) (
    -x[i]+2*x[j]-x[k] + 19*d[i,j,k] <= 18
  )
;


output [
  show(tot), "\n", 
  show(x), "\n"
];

