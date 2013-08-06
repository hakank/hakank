% 
% Safe cracking puzzle in MiniZinc.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

var 1..9: C1;
var 1..9: C2;
var 1..9: C3;
var 1..9: C4;
var 1..9: C5;
var 1..9: C6;
var 1..9: C7;
var 1..9: C8;
var 1..9: C9;

array[1..9] of var 1..9: LD = [C1,C2,C3,C4,C5,C6,C7,C8,C9];

solve satisfy;

constraint
  all_different(LD) /\
  C4 - C6 = C7 /\
  C1 * C2 * C3 = C8 + C9 /\
  C2 + C3 + C6 < C8 /\
  C9 < C8 /\
  forall(i in 1..9) (
    LD[i] != i
  )
;




output 
[
  show(LD)
];