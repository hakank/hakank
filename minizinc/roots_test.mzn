% 
% Global constraint roots in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Croots.html
% """
% roots​(S,​T,​VARIABLES)
%
% Purpose
%
% S is the set of indices of the variables in the collection VARIABLES 
% taking their values in T; S =​{i | VARIABLES[i].var in T}.
%
% Example
%    (
%    ​{2, 4, 5},
%    ​{2, 3, 8},
%    <1, 3, 1, 2, 3>
%    )
%
% The roots constraint holds since values 2 and 3 in T occur in the collection 
% <1, 3, 1, 2, 3> only at positions S={2, 4, 5}. The value 
% 8 in T does not occur within the collection <1,​3,​1,​2,​3>.
%
% """

% Note: The global constraint roots is included in globals.mzn, distributed
% with the MiniZinc package

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..3: x;
var set of 1..5: s;
var set of 1..8: t;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(x, [1,3,1,2,3])
  /\
  s = {2,4,5}
  /\
  t = {2,3,8}
  /\
  roots(x, s, t)
;


output [
"x: " ++ show(x) ++ "\n"++
"s: " ++ show(s) ++ "\n"++
"t: " ++ show(t) ++ "\n"
];
