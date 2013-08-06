% 
% in MiniZinc.
% 
% From
% Travels in a mathematical world
% "A puzzle from James Grime about abcdef"
% http://travels.peterrowlett.net/2012/02/puzzle-from-james-grime-about-abcdef.html
% """
% Today James Grime tweeted this question/puzzle:
% 
% Is there a six digit number abcdef such that the following all hold?
%
%    a+b+c+d+e+f = y
%    ab+cd+ef=10y
%    abc+def=100y
%
% If not, show why not.
%
% A little tweeting back and forth verified that "ab" means 10a+b not a×b. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

var 1..9: a;
var 0..9: b;
var 0..9: c;
var 0..9: d;
var 0..9: e;
var 0..9: f;

var int: y;

array[1..6] of var 0..9: x = [a,b,c,d,e,f];


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   
   y = sum(x) 
   /\
   (10*a+b) + (10*c+d) + (10*e+f) = 10*y
   /\
   (100*a+10*b+c) + (100*d+10*e+f) = 100*y
;

output [
  "x: " ++ show(x) ++ " y: " ++ show(y) ++ "\n"
]
 ++ ["\n"]
;

