% 
% Portfolio optimization in MiniZinc.
% 
% Example from
% SAS/OR(R) 9.2 User's Guide: Mathematical Programming:
% http://support.sas.com/documentation/cdl/en/ormpug/59679/HTML/default/qpsolver_sect13.htm

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
float: B = 10000.0;
float: G = 1000.0;
array[1..n] of float: r = [0.05, -0.2,0.15,0.30];

array[1..n, 1..n] of float: coeff = 
  array2d(1..n,1..n,
  [
    0.08,-0.05,-0.05,-0.05,
   -0.05, 0.16,-0.02,-0.02,
   -0.05,-0.02, 0.35, 0.06,
   -0.05,-0.02, 0.06, 0.35
  ]);

array[1..n] of var 0.0..B: x;
var float: f;

% solve satisfy;
solve :: float_search(x, 0.01, first_fail, indomain_split, complete) satisfy;

constraint
   %forall(i in 1..n) (
   %   x[i] >= 0.0 
   %)
   %/\
   f = sum(i, j in 1..n) (
            % here is the problem! (nonlinear)
            coeff[i,j]*x[i]*x[j]
       )  
   /\
   sum(i in 1..n) (x[i]) <= B /\
   sum(i in 1..n) (r[i]*x[i]) >= G
;

output 
[
 "x: ", show(x), "\n",
 "f: ", show(f), "\n"
];