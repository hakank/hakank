% 
% Global constraint soft_all_equal_ctr in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csoft_all_equal_ctr.html
% """
% Constraint
%
%  soft_all_equal_ctr​(VARIABLES,​N)
%
% Purpose
%
% Consider the equality constraints involving two distinct variables of 
% the collection VARIABLES. Among the previous set of constraints, N is 
% less than or equal to the number of equality constraints that hold.
% 
% Example
%  ​(6, <5, 1, 5, 5>)
%
% Within the collection <5, 1, 5, 5> six equality constraints holds. 
% Consequently, the soft_all_equal_ctr constraint holds since the 
% argument N=6 is less than or equal to the number of equality 
% constraints that hold.
% 
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..5: x;
var int: N;

predicate soft_all_equal_ctr(array[int] of var int: x, var int: N) = 
  let {
    int: len = length(x)
  }
  in
  N >= 0
  /\ 
  N <= len * len
  /\
  N <= sum(i,j in 1..len where i != j) (
     bool2int(x[i] = x[j])
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(x, [5,1,5,5])
  /\
  N = 6
  /\
  soft_all_equal_ctr(x, N)  
;

output ["x: \(x) n:\(N)"];
