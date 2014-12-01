% 
% Bertrand Russell puzzle in MiniZinc.
% 
% From http://stackoverflow.com/questions/26809266/bertrand-russell-puzzle
% """
% As a simple exercise in abstraction suppose that four meaningless
% symbols a, b, c, and d correspond in one order or another to the
% equally meaningless symbols w, x, y, and z, and suppose further that
%
%     If a is not x, then c is not y.
%     If b is either y or z, then a is x.
%     If c is not w, then b is z.
%     If d is y, then b is not x.
%     If d is not x, then b is x.
%
% In what order do the two sets of symbols correspond?
% """
% 
% (The problem is also in "101 puzzles in Thought and Logic", page 58)

% With the symmetry breaking that abcd = 1..4, this problem has one solution:
%
%  a<=>y
%  b<=>x
%  c<=>w
%  d<=>z


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;

int: a = 1;
int: b = 2;
int: c = 3;
int: d = 4;

array[1..n] of var 1..n: abcd = [a,b,c,d];
array[1..n] of string: abcd_s = ["a","b","c","d"];

var 1..n: w;
var 1..n: x;
var 1..n: y;
var 1..n: z;
array[1..n] of var 1..n: wxyz = [w,x,y,z];
array[1..n] of string: wxyz_s = ["w","x","y","z"];

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(wxyz) /\   

  % If a is not x, then c is not y.
  (a != x -> c != y) /\

  % If b is either y or z, then a is x.
  ( ( (b = y) \/ (b = z)) -> a = x) /\

  % If c is not w, then b is z.
  ( c != w -> b = z) /\

  % If d is y, then b is not x.
  ( d = y -> b != x) /\

  % If d is not x, then b is x.
  ( d != x -> b = x)

;

output [

  show([a,b,c,d]) ++ "\n" ++
  show([w,x,y,z]) ++ "\n"
]
++
[
  if fix(wxyz[j]) = i then
   show(join("<=>", [abcd_s[i], wxyz_s[j]])) ++ "\n"
  else 
    ""
  endif
  | i in 1..n, j in 1..n

];

