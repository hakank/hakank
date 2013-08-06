%
% Torn number puzzle in Minizinc.
%
% Problem statement and model inspiration from
% """
% http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/torn.html
% The Torn Number    from "Amusements in Mathematics, Dudeney", number 113
% 
% I had the other day in my possession a label bearing the number 3025
% in large figures. This got accidentally torn in half, so that 30 was
% on one piece and 25 on the other. On looking at these pieces I began
% to make a calculation, scarcely concious of what I was doing, when I
% discovered this little peculiarity. If we add the 30 and the 25
% together and square the sum we get as the result the complete original
% number on the label! Now, the puzzle is to find another number,
% composed of four figures, all different, which may be divided in the
% middle and produce the same result.
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

var 0..9: D0;
var 0..9: D1;
var 0..9: D2;
var 0..9: D3;
var int: Sum;

solve satisfy;

constraint
   D3 != 0 /\
   Sum = D3 * 10 + D2 + D1 * 10 + D0 /\
   Sum * Sum = D3 * 1000 + D2 * 100 + D1 * 10 + D0 /\
      not (30 = D3*10 + D2  \/ 30 = D1*10 + D0)  /\ % none of the bits a30
   not (25 = D3*10 + D2  \/ 25 = D1*10 + D0)  % none of the bits are 25
   /\ all_different([D0,D1,D2,D3])
;

output [
   show(D0), " ", show(D1), " ", show(D2), " ", show(D3), "\n"

];



