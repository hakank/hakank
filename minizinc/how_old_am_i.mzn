% 
% How old am I puzzle in MiniZinc.
% 
% From Choco: samples/Examples/HowOldAmI.java:
% """
% Easy simple problem defined by:
% "Six years ago, my brother was two time my age.
% In five years, we will have 40 years together.
% How old am I?"
% (sorry for the translation :) )
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var 0..40: me;
var 0..40: him;

solve satisfy;

constraint
  
        2*(me-6) = him-6
        /\
        40 = (me+5) + (him+5)
;

output
[
  show([me, him])
];