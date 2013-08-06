% 
% Dudeney's bishop placement problem II in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
%
% http://www.chlond.demon.co.uk/puzzles/puzzles2.html, puzzle nr. 8 
% Description  : Dudeney's bishop placement problem II
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s8.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 8;
array[1..size, 1..size] of var 0..1: x; % x(i,j) = 1 if square (I,J) occupied, 0 otherwise
array[1..size, 1..size] of var 0..1: a; % a(i,j) = 1 if square (I,J) attacked, 0 otherwise

var int: sumx = sum(i in 1..size,j in 1..size) (x[i,j]);


% maximise number of bishops
solve :: int_search([x[i,j] | i,j in 1..size], first_fail, indomain_min, complete)  maximize sumx;
% solve maximize sumx;


constraint
  % a[i,j] = 1 if square (i,j) attacked
  forall(i in 1..size,j in 1..size) (
   sum(m in 1..size where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
   sum(m in 1..size where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) <= 99*a[i,j]
  )
  /\
  % each square either attacked or occupied
  forall(i in 1..size,j in 1..size) (
    a[i,j]+x[i,j] = 1
  )
;

output 
["\nsumx: ", show(sumx) ] ++
["\nx:"] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..size
] ++
["\na:"] ++
[
  if j = 1 then "\n" else " " endif ++
    show(a[i,j])
  | i,j in 1..size
] ++ ["\n"]
;
