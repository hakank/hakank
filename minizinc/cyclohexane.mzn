%
% Standard non linear problem in Minizinc
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Compare with 
%   * Real Paver
%   * Choco

% solution in realpaver:
% x in [-10.85770359962651 , -10.85770359962645]
% y in [-0.7795480450791618 , -0.7795480450791532]
% z in [-0.77954804507916 , -0.7795480450791551]

% 
% eclipse fzn_ic gives a couple of solutions but is very slow (about 5 minutes)
% z = 0.77948920403276278__0.77953408925010714
% y = 10.857126425686687__10.85815035306104
% x = 0.7795642647571428__0.7796111045423918
% Total time 224.980s cpu (0.010 setup + 224.970 search)
%

% 
% var -20.0..20.00: x;
% var -20.0..20.00: y;
% var -20.0..20.00: z;

var float: x;
var float: y;
var float: z;


solve satisfy;
% solve :: float_search([x,y,z], 0.0001, "input_order", "indomain_split", "complete")  satisfy;

constraint
        13.0 + y*y*(1.0+z*z) + z*(z - 24.0*y)  = 0.0 /\
        13.0 + z*z*(1.0+x*x) + x*(x - 24.0*z)  = 0.0 /\
        13.0 + x*x*(1.0+y*y) + y*(y - 24.0*x)  = 0.0 
 ;


output [
"x: " ++ show(x) ++ "\n"++
"y: " ++ show(y) ++ "\n"++
"z: " ++ show(z) ++ "\n"
];
