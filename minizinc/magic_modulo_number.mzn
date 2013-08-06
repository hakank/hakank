% 
% Magic Modulo Number in MiniZinc.
% 
% From the Scampi example MagicModuloNumber.scala
% """
% A number with an interesting property:
% 
% When I divide it by  2, the remainder is 1.
% When I divide it by  3, the remainder is 2.
% When I divide it by  4, the remainder is 3.
% When I divide it by  5, the remainder is 4.
% When I divide it by  6, the remainder is 5.
% When I divide it by  7, the remainder is 6.
% When I divide it by  8, the remainder is 7.
% When I divide it by  9, the remainder is 8.
% When I divide it by 10, the remainder is 9.
% 
% It's not a small number, but it's not really big, either.
% When I looked for a smaller number with this property I couldn't find one.
% Can you find it?
% 
% @author Pierre Schaus pschaus@gmail.com
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

% var 0..10000: x;
var 0..10000000: x;

solve satisfy;
% solve minimize x;

constraint
   forall(i in 2..10) (
      x mod i = i-1
   )
;

output [
 "x: " ++ show(x)
]
 ++ ["\n"]
;

