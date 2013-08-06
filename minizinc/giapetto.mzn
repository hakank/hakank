%
% Giapetto's problem in Minizinc
%
%
% From the article
% The GNU Linear Programming Kit, Part 1: Introduction to linear optimization
% http://www-128.ibm.com/developerworks/library/l-glpk1/index.html?ca=drs-
% 
% (Translation of the GLPK MathProg model.)
%
% This finds the optimal solution for maximizing Giapetto's profit.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Decision variables
var int: x1; % soldier
var int: x2; % train 

% Objective function
solve maximize 3*x1 + 2*x2;

% Constraints
constraint
  x1 >= 0 /\ x2 >= 0 /\
  2*x1 + x2 <= 100   /\ % Finishing 
  x1 + x2 <= 80      /\ % Carpentry
  x1 <= 40               % Demand
;


output
[
  "x1: " ++ show(x1) ++ "\n" ++
  "x2: " ++ show(x2) ++ "\n"
];
