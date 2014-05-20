% 
% Catenary in MiniZinc.
% 
% Translated from the AMPL model
% http://orfe.princeton.edu/~rvdb/ampl/nlmodels/catenary/index.html

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% Objective: linear
% Constraints: convex quadratic
% Feasible set: convex

% This model finds the shape of a hanging chain
% The solution is known to be y = cosh(a*x) + b
% for appropriate a and b.

int: N = 100;	% number of chainlinks
float: NN = int2float(N);
float: L = 1.0;	% difference in x-coords of endlinks

float: h = 2.0*L/NN;	% length of each link

array[0..N] of float: x = array1d(0..N,[int2float(i)*L/NN | i in 0..N]);	% x-coordinates of endpoints of chainlinks
array[0..N] of var float: y;	% y-coordinates of endpoints of chainlinks

% pot_energy:
var float: z = sum(j in 1..N) ((y[j-1] + y[j])/2.0);

% solve satisfy;
solve minimize z; % to get a single solution
% solve :: float_search(y, 0.01, input_order, indomain_split, complete) minimize z;

constraint
   x[0] = 0.0 /\ % x_left_anchor
   y[0] = 0.0 /\ % y_left_anchor
   x[N] = L /\
   y[N] = 0.0 /\

   forall(j in 1..N) (
        (x[j] - x[j-1])*(x[j] - x[j-1]) + (y[j] - y[j-1])*(y[j] - y[j-1]) <= pow(h,2.0)
   )
   % /\ forall(j in 0..N) (x[j] = int2float(j)*L/NN) 


;

output 
[ 
  "z: " ++ show_float(3,2,z) ++ "\n"
]
++
[
  show_float(3,2,x[j])++ " " ++ show_float(3,2,y[j]) ++ "\n"
  | j in 0..N
];

