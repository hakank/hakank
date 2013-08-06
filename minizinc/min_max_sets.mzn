% 
% Minimum and maximum values in a set in MiniZinc.
% 
% Here is an example of min_set/2 and max_set/2 which
% gives the minimum and maximum values in a set.
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 10;

var set of 1..n: S;
var 1..n: mn; % minimum value of the set
var 1..n: mx; % maximum value of the set

% m is the minimum value of the set s
predicate min_set(var set of int: s, var int: m) =
   m in s
   /\
   forall(e in ub(s)) (
      (e in s) -> m <= e
   )
;

% m is the maximum value of the set s
predicate max_set(var set of int: s, var int: m) =
   m in s
   /\
   forall(e in ub(s)) (
      (e in s) -> m >= e
   )
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   card(S) = 4
;

constraint
  min_set(S, mn) 
  /\
  max_set(S, mx)
;

% constraint
%    /\
%    mn in S
%    /\
%    forall(e in 1..n) (
%       (e in S) -> mn <= e
%    )

% ;

output [
  "S  : " ++ show(S) ++
  "min: " ++ show(mn) ++
  "max: " ++ show(mx)
]
 ++ ["\n"]
;

