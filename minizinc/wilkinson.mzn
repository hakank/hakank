% 
% Wilkinson's problem (non linear) in MiniZinc.
%
% Non linear problem from RealPaver's example wilkinson.
%
% Solution in RealPaver
% OUTER BOX 10
%   x in [-20.84690810148226 , -20.84690810148225]
%
%
% As of writing (2010-07-21) the only solver that can
% solve this is ECLiPSe/ic:
% 
%   x = -20.846182797708117__-20.846121006743424;
%
% Later: Both Gecode and JaCoP solves this problem:
%    -20.84690810148

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var -100.00..10.00: x; 

solve minimize x;
% solve::float_search([x], 0.000001, input_order, indomain_split, complete) minimize x;

output [
 show(x)
];

constraint
%   x >= -100.0 /\ x <= 10.0
%   /\
  0.0 = (x+1.0)*(x+2.0)*(x+3.0)*(x+4.0)*(x+5.0)*(x+6.0)*(x+7.0)*(x+8.0)*(x+9.0)*(x+10.0)*(x+11.0)*(x+12.0)*(x+13.0)*(x+14.0)*(x+15.0)*(x+16.0)*(x+17.0)*(x+18.0)*(x+19.0)*(x+20.0)
      + 0.00000011920928955078*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x*x
                              % this is x**19
;





