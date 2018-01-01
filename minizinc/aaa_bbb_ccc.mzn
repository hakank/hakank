% 
% AAA + BBB + CCC = BAAC in MiniZinc.
% 
% https://mindyourdecisions.com/blog/2017/12/27/puzzle-if-aaa-bbb-ccc-baac-what-are-a-b-and-c/
% """
% If AAA + BBB + CCC = BAAC, What Are A, B, And C?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

var 1..9: A;
var 1..9: B;
var 1..9: C;

var int: AAA = A*100+A*10+A;
var int: BBB = B*100+B*10+B;
var int: CCC = C*100+C*10+C;
var int: BAAC = B*1000+A*100+A*10+C;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   BAAC = AAA+BBB+CCC /\
   all_different([A,B,C])
;

% output [];

