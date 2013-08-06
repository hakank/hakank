%
% Linear programming in Minizinc
% 
% From the AMPL book, page 5
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var float: XB; 
var float: XC; 

solve maximize 25.0 * XB + 30.0 * XC; 

constraint
  XB >= 0.0 
  /\ 
  XC >= 0.0 
  /\
  XB <= 6000.0 
  /\ 
  XC <= 4000.0
  /\
  (1.0/200.00) * XB + (1.0/140.0) * XC <= 40.0
;

output [show([XB,XC])];