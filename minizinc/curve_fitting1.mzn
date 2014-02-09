% 
% Curve fitting in MiniZinc.
% 
% From GLPK example cf12a.mod:
% """
% Curve fitting problem 12.11(a) H P Williams "Model Building in 
% Mathematical Programming"
% """
%
% The answer should be 
%    y = 0.6375x + 0.581

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n;

% independent variable
array[1..n] of float: x;

% dependent variable
array[1..n] of float: y;

var 0.0..1.0: a;
var 0.0..1.0: b;

array[1..n] of var 0.0..3.0: u;
array[1..n] of var 0.0..3.0: v;

var 0.0..30.0: z = sum (i in 1..n) (u[i]) + sum(i in 1..n) (v[i]);

solve :: float_search(u++v,0.00001, input_order, indomain_split, complete) minimize z;
% solve minimize z;

constraint
  a >= 0.0 /\ b>= 0.0 /\
  forall(i in 1..n) (
        u[i] >= 0.0
        /\
        v[i] >= 0.0
        /\
        b * x[i] + a + u[i] - v[i] = y[i]
  )


;


%
% data;
% 
n = 19;
x = [0.0, 0.5,1.0,1.5,1.9,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.6,7.0,7.6,8.5,9.0,10.0];
y =  [1.0,0.9,0.7,1.5,2.0,2.4,3.2,2.0,2.7,3.5,1.0,4.0,3.6,2.7,5.7,4.6,6.0,6.8,7.3];

output 
[
 "z: ", show(z), "\n",
 "y = ", show(b), "*x + ", show(a), "\n",
 "u: " ++ show(u), "\n",
 "v: " ++ show(v), "\n"

];
