%
% Dynamical Optimization in Minizinc
% 
% From
% http://www.dcsc.tudelft.nl/%7Eahuesman/Ampl.pdf (solution page 8)
% http://www.dcsc.tudelft.nl/~ahuesman/
%
% Solves the problem
%   min u(t) int(x dt, 0,10)
%   s.t. 
%     dx/dt = -x(t) + u(t)
%     x(0) = 1
%     x(10) = 5
%     0 <= u(t) <= 10  
%
% Translation of the AMPL model.

% From the documentation
% """"
% DOP1.mod - Simple problem to test dynamic optimization in AMPL format
% Adrie Huesman 27/11/2004
% Problem taken from "Research note: Solving and optimization of ODE.s"
% SETS
% no set declaration necessary!
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: Ng; % number of grid points
float: Dt; % time step

% Variables
array[1..Ng] of var 0.00..10.00: x;
array[1..Ng] of var 0.00..10.00: u;

var float: cost = sum(j in 1..Ng) (x[j]);

% Objective
% solve minimize cost; % discrete integral approximation
solve :: float_search(x ++ u,0.0001,input_order,indomain_split,complete) minimize cost; % discrete integral approximation

% Constraints
constraint
  forall(j in 1..Ng-1) (
     (x[j+1] - x[j])/Dt = -x[j+1] + u[j+1]
  )
  /\
  % implicit Euler approximation
  x[1] = 1.0 % initial condition
  /\
  x[Ng] = 5.0 % end condition

;

%
% Data
%
Ng = 101;
Dt = 0.1; % (10 - 0)/(Ng - 1)

output [
"x: " ++ show(x) ++ "\n"++
"u: " ++ show(u) ++ "\n"++
"cost: " ++ show(cost) ++ "\n"
];
