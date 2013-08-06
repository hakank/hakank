% 
% Equal Vision puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html, puzzle nr. 12.  
% Description  : Equal Vision
% Source       : Poniachik, J. & L., (1998), Hard-to-solve Brainteasers, Sterling

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s12.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 4;
int: cvis = 6;
set of 1..size: S = 1..size;
set of 1..cvis: C = 1..cvis;

% x(i,j) = 0 if cell {i,j} occupied, 1 otherwise
array[S,S] of var 0..1: x; 
% n(i,j) = number of vacant cells visible to watchman on cell {i,j}
array[S,S] of var 0..20: n; % 0..4*(size-4): n; 

var int: sumx = sum(i in S,j in S) (x[i,j]);

% minimise vacant cells
solve :: int_search([x[i,j] | i,j in S], first_fail, indomain_min, complete) minimize sumx;

constraint 

  forall(i in S,j in S) (
        sum(m in S where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
        sum(m in S where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) +
        sum(m in S where m != i) (x[m,j]) + 
        sum(m in S where m != j) (x[i,m]) = n[i,j]
  )
  /\

  forall(i in S,j in S) (
     n[i,j] >= cvis-99*x[i,j]
  )
  /\
  forall(i in S,j in S) (
      n[i,j] <= cvis+99*x[i,j]
  )

;

output [
   if i = 1 /\ j = 1 then
    "\nsumx: " ++ show(sumx)
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i in S, j in S
] ++ ["\n"];
