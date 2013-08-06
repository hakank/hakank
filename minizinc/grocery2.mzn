% 
% Grocery problem (alternative model) in MiniZinc.
% 
% """
% A kid goes into a grocery store and buys four items. The cashier charges $7.11. 
% The kid pays and is about to leave when the cashier calls the kid back, and says 
% "Hold on, I multiplied the four items instead of adding them; I'll try again... 
% Gosh, with adding them the price still comes to $7.11"! What were the prices of 
% the four items?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 1..711: A;
var 1..711: B;
var 1..711: C;
var 1..711: D;

var int: T1;
var int: T2;

% solve satisfy;
solve :: int_search([A,B,C,D, T1,T2], first_fail, indomain_min, complete) satisfy;

constraint
  % symmetry breaking
  A <= B /\
  B <= C /\
  C <= D /\
  
  A+B+C+D = 711 /\
  A*B = T1 /\
  C*D = T2 /\
  T1*T2 = 711*100*100*100
;

output [
  show([A,B,C,D]), "\n",

];