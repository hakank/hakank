% 
% Curve fitting in MiniZinc.
% 
% From GLPK example cflsq.mod
% """
% Curve fitting problem by Least Squares
% """
%
% The answer should be 
%    y = 0.426126 + 0.610772x


% Note:
% Only ECLiPSe/ic, Gecode and JaCoP can handle var float arrays and float times.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n;
set of int: Sample = 1..n;

% independent variable
array[Sample] of float: Sx;

% dependent variable
array[Sample] of float: Sy;

var float: X;
var float: Y;

array[Sample] of var float: Ex;
array[Sample] of var float: Ey;

var float: b1 = (sum(z in Sample) (Ex[z]*Ey[z]))/(sum(z in Sample) (Ex[z]*Ex[z]));


% solve satisfy;
solve :: float_search(Ex++Ey,0.000000000001, input_order, indomain_split, complete) satisfy;

constraint
  % sum of variances is zero for Sx
  sum(z in Sample) (Ex[z]) = 0.0
  /\
  forall(z in Sample) (X + Ex[z] = Sx[z])
  /\
  % sum of variances is zero for Sy
  forall(z in Sample) (Y + Ey[z] = Sy[z])
  /\
  sum(z in Sample) (Ey[z]) = 0.0

;


%
% data;
% 
n = 19;
Sx = [0.0, 0.5,1.0,1.5,1.9,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.6,7.0,7.6,8.5,9.0,10.0];

Sy = [1.0,0.9,0.7,1.5,2.0,2.4,3.2,2.0,2.7,3.5,1.0,4.0,3.6,2.7,5.7,4.6,6.0,6.8,7.3];

output 
[
  "\nbest linear fit is:\n\ty = ", show(Y-b1*X), " + ", show(b1), "\n",
];
