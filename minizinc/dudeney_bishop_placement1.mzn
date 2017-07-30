% 
% Dudeney's Bishop Placement problem 1 in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
%
% http://www.chlond.demon.co.uk/puzzles/puzzles1.htm2, puzzle nr. 7
% Description  : Dudeney's bishop placement problem I
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol2s7.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: size = 8;

array[1..size, 1..size] of var 0..1: x; % x(i,j) = 1 if square (I,J) occupied, 0 otherwise
array[1..size, 1..size] of var 0..1: a; % a(i,j) = 1 if square (I,J) attacked, 0 otherwise

% array[1..size, 1..size] of var 0..1: check;

var int: sumx = sum(i in 1..size,j in 1..size) (x[i,j]);

% minimize number of bishops
% solve :: int_search([x[i,j] | i,j in 1..size], first_fail, indomain, complete)  minimize sumx;
solve :: int_search(
        [x[i,j] | i,j in 1..size] ++  [a[i,j] | i,j in 1..size], 
        first_fail, 
        indomain_min, 
        complete)  
    minimize sumx;



constraint
  % a(i,j) = 0 if square (i,j) not attacked
  forall(i in 1..size,j in 1..size) (
   (
    sum(m in 1..size where m != i /\ m-i+j >= 1 /\ m-i+j <= size) (x[m,m-i+j]) +
    sum(m in 1..size where m != i /\ i+j-m >= 1 /\ i+j-m <= size) (x[m,i+j-m]) 
   ) >= a[i,j] 
  )
  /\
  % each square is either attacked or occupied
  forall(i in 1..size,j in 1..size) (
     % check[i,j] = a[i,j]+x[i,j] 
     % /\ 
     % check[i,j] = 1
     a[i,j]+x[i,j] = 1
  )
;

% Testing
% constraint sumx = 8;

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
]
%  ++ 
% ["\ncheck:"] ++
% [
%   if j = 1 then "\n" else " " endif ++
%     show(check[i,j])
%   | i,j in 1..size
% ] ++ ["\n"]
;
