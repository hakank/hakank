% 
% Sum the integers in a set in MiniZinc.
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 10;
var set of 1..n: s;
var int: s_total;

predicate sum_set(var set of int: ss, var int: total) =
  let {
    int: m = card(ub(ss)), 
    array[1..m] of var 0..1: tmp
  }
  in
  forall(i in 1..m) (
    i in ss <-> tmp[i] = 1
  )
  /\
  total = sum(i in 1..m) (i*tmp[i])
;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   sum_set(s, s_total)
;

output [
"s: " ++ show(s) ++ "\n"++
"s_total: " ++ show(s_total) ++ "\n"
];
