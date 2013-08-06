%  
% The Abbott's Window puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html, puzzle 2 The Abbott's Widow.
% Description  : The Abbott's Window
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s2.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


int: row = 8;
int: col = 8;
  
set of 1..row: R = 1..row;
set of 1..col: C = 1..col;
array[R,C] of var 0..1: x ;
array[R] of var 0..4: a;
array[C] of var 0..4: b;
array[1..row-2] of var 0..4: c;
array[1..col-1] of var 0..4: d; 
array[1..col-1] of var 0..4: e; 
array[1..row-2] of var 0..4: f;

var int: total_sum = sum(i in R,j in C) (x[i,j]);

constraint
   forall(i in R) (
        sum(j in C) (x[i,j]) = 2*a[i]
   )
;
    
constraint 
     forall(j in C) (
        sum(i in R) (x[i,j]) = 2*b[j]
     )
;

constraint 
    forall(i in 2..row-1) (
      sum(k in 1..i) (x[k,i-k+1]) = 2*c[i-1]
    )
;
    
constraint 
    forall(j in 1..col-1) (
        sum(k in j..row) (x[k,col-k+j]) = 2*d[j]
    )
;

constraint 
   forall(j in 1..col-1) (
        sum(k in 1..row-j+1) (x[k,j+k-1]) = 2*e[j]
   )
;

constraint 
   forall(i in 2..row-1) (
        sum(k in i..row) (x[k,k-i+1]) = 2*f[i-1]
   )
;

constraint 
 x[1,1] = 1 /\
 x[row,1] = 1 /\
 x[1,col] = 1 /\
 x[row,col] = 1 
;

% solve satisfy;
solve :: int_search([x[i,j] | i in R, j in C], first_fail, indomain_min, complete) maximize total_sum;
% solve maximize total_sum;


output [
    if i = 1 /\ j = 1 then
       "total_sum: " ++ show(total_sum) ++ "\n"
       else "" endif ++
       if j = 1 then show(i) ++ " : " else "" endif ++
       show(x[i,j]) ++ if j = col then "\n" else " " endif
    | i in R, j in C
];
