% 
% Enigma puzzle #1615 in MiniZinc.
% 
% 
% Via http://www.f1compiler.com/samples/Enigma%201615.f1.html
% """  
% Enigma 1615, Gwyn Owen, New Scientist magazine, 29 September 2010
% ENIGMA represents a six-digit number in which the digits are 
% all different and non-zero.
% 
% When EN slides from the front to the back, the resulting six-digit 
% number, IGMAEN, is 20 per cent larger than ENIGMA.
% 
% What is ENIGMA? 
% """
% 
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: N = 6;

var 1..9: e;
var 1..9: n;
var 1..9: i;
var 1..9: g;
var 1..9: m;
var 1..9: a;

var 100000..999999: enigma;
var 100000..999999: igmaen;


solve satisfy;

constraint
    alldifferent([e,n,i,g,m,a]) /\

    enigma = e*100000 + n*10000 + i*1000 + g*100 + m*10 + a /\
    igmaen = i*100000 + g*10000 + m*1000 + a*100 + e*10 + n /\
    10*igmaen = 12*enigma
;

output [
  "enigma: " ++ show(enigma) ++ "\n" ++
  "igmaen: " ++ show(igmaen) ++ "\n"

];

