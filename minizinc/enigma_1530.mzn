% 
% Enigma problem 1530 (Tom Daley) in MiniZinc.
% 
% From New Scientist:
% """
% Tom Daley
%
% Enigma 1530 Richard England, New Scientist magazine, January 28, 2009.
% 
% Before being the youngest member of the British team at the 2008 Beijing 
% Olympics, Tom Daley had become the youngest European diving champion on 
% record by winning the individual title from the 10-metre platform board 
% while still aged only 13.
% 
% So it is appropriate that I can offer this puzzle:
% TOM * 13 = DALEY
% 
% Each letter stands for a different digit, and no number starts with a zero.
% What is the five-digit number represented by DALEY?
% """
%
% Compare with the Formula One model:
% http://www.f1compiler.com/samples/Enigma%201530.f1.html
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 8;
set of int: Digits = 0..9;
var Digits: T;
var Digits: O;
var Digits: M;
var Digits: D;
var Digits: A;
var Digits: L;
var Digits: E;
var Digits: Y;

array[1..n] of var Digits: x = [T,O,M,D,A,L,E,Y];
var 10000..99999: DALEY;
var 100..999: TOM = 100*T + 10*O + M;

% convert array <-> number
predicate toNum(array[int] of var int: a, var int: n) =
          let { int: len = length(a) } in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   all_different(x) /\
   T > 0 /\ D > 0 /\
   toNum([D,A,L,E,Y], DALEY) /\
   TOM * 13 = DALEY
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "DALEY: " ++ show(DALEY)
];
