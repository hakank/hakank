% 
% Global constraint ith pos different from 0 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cith_pos_different_from_0.html
% """
% ith_pos_different_from_0​(ITH,​POS,​VARIABLES)
%
% Purpose
% POS is the position of the ITHth non-zero item of the sequence of variables 
% VARIABLES.
% 
% Example
%    ​(2,​4,​〈3,​0,​0,​8,​6〉)​
%
% The ith_pos_different_from_0 constraint holds since 4 corresponds to the 
% position of the 2th non-zero item of the sequence 3 0 0 8 6.
% 
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..8: x;
var 1..n: ith;
var 1..n: pos;

predicate ith_pos_different_from_0(var int: ith, 
                                   var int: pos, 
                                   array[int] of var int: x) =
   let {
        int: lbx = min(index_set(x)),
        int: ubx = max(index_set(x))
   } in
   exists(i in lbx..ubx) (
     pos = i 
     /\
     ith = sum(j in lbx..i) (bool2int(x[j] != 0))
     /\
     x[i] != 0
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   cp1d(x,[3,0,0,8,6])
   /\
   pos = 4
   /\
   ith = 2
   /\
   ith_pos_different_from_0(ith, pos, x)
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "pos: " ++ show(pos) ++ "\n" ++
  "ith: " ++ show(ith) ++ "\n"
 
];
