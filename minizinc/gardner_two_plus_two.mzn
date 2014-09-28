% 
% Gardner's TWO+TWO=FOUR problem in MiniZinc.
% 
% Via OscaR's GardnerTwoPlusTwo program:
% """
% Martin Gardner Problem:
% 
% In this addition problem, each letter stands for a different digit.
% 
%    T W O
%  + T W O
%  --------
%  F O U R
% 
% If T = 7 and the letter O represents an even number, what is the only 
% possible value for W
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 


var 7..7: T; % hardwired to 7
var 0..9: W;
var 0..9: O;
var 1..9: F; % > 0
var 0..9: U;
var 0..9: R;

array[1..6] of var 0..9: FD = [T,W,O,F,U,R];

solve satisfy;

constraint
   alldifferent(FD)
   /\
   2 * (100*T + 10*W + O) = 1000*F + 100*O + 10*U + R
   /\  
   O mod 2 = 0
;

output [
       
       show(T), show(W), show(O), " + ", show(T), show(W), show(O), " = ",
       show(F), show(O), show(U), show(R), "\n",
       "W: ", show(W), "\n"
]
