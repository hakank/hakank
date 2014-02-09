% 
% Rosenbrock function (a nonlinear standard problem) in MiniZinc.
% 
% This is problem 3.1 from
% http://www.cs.cas.cz/ics/reports/v798-00.ps
% """
% converted to AMPL by Yu-Ju Kuo and Hans D. Mittelmann
% """
%
% Also see:
% http://mathworld.wolfram.com/RosenbrockFunction.html
% http://en.wikipedia.org/wiki/Rosenbrock_function
% """
% It is also known as Rosenbrock's valley or Rosenbrock's banana function.
% It has a global minimum at (x,y) = (1,1) where f(x,y) = 0.
% """
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% ECLiPSe/ic finds a solution i 31 seconds:
% Found a solution with cost 0.00011437126232709975__0.00011543886261273339
% Found no solution with cost -52014.0 .. -0.99988456113738722
% x1 = 0.98925626464338035__0.98930532830499018
% x2 = 0.97862757926386235__0.97872465474033288
% Minimum objective value = 0.00011437126232709975__0.00011543886261273339
% Total time 29.080s cpu (0.000 setup + 29.080 search)

var -1.0..8.0: x1;
var -1.0..8.0: x2;
var float: z =   100.0*(x2-x1*x1)*(x2-x1*x1)+(1.0-x1)*(1.0-x1);

solve :: float_search([x1,x2], 0.0001, input_order, indomain_split,complete) minimize 
  z
  % 105.0*(x2-x1*x1)*(x2-x1*x1)+(1.0-x1)*(1.0-x1) % variation
;




output 
[
 show([x1,x2,z])
];