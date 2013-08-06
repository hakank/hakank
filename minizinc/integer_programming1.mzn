%
% Integer programming in Minizinc
%
% Problem from http://forums.topcoder.com/;jsessionid=106E29747627F66C41B2F1FC31AEF971?module=Thread&threadID=610243&start=0&mc=4#961241
%
% """
% maximize ab + bc
% 
% subject to
% 
% 1<=a<=10
% 1<=b<=10
% 1<=c<=10
% 
% a,b,c are integers and unique (not equal)
% """
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

var 1..10: a;
var 1..10: b;
var 1..10: c;

var int: z =  a*b + b*c;

solve maximize z;
% solve satisfy;

constraint 
  % z >= 170 /\
  all_different([a,b,c])
;

output
[
  show([a,b,c])
];
