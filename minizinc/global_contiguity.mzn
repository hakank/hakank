% 
% Global constraint global_contiguity in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cglobal_contiguity.html
% """
% Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
% In addition, all variables assigned to value 1 appear contiguously.
% 
% Example:
% (<0,​1,​1,​0>)
% 
% The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
% no more than one group of contiguous 1.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 0..1: x;
var 1..n: start;
var 1..n: end;

%
% Plain global_contiguity, but _pars is better since we have
% the control over start and end of the 1's
%
predicate global_contiguity(array[int] of var 0..1: x) =
  let { 
    int: len = length(x),
    int: lbx = min(index_set(x)),
    int: ubx = max(index_set(x)),
    var lbx..ubx: xstart,
    var lbx..ubx: xend
  }
  in
  global_contiguity_pars(x, xstart, xend)
;


%
% global_contiguity with parameters start, end
%
predicate global_contiguity_pars(array[int] of var 0..1: x, var int: xstart, var int: xend) =

  forall(i in index_set(x)) (
     (i >= xstart /\ i <= xend) <-> x[i] = 1
  )
  /\
  xend >= xstart
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  %cp1d(x, [0,1,1,0])
  %/\
  % global_contiguity(x)
  global_contiguity_pars(x,start, end) 
  %/\
  %start = 2
  %/\
  %end = 3
;

output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "start: " ++ show(start) ++ "\n" ++ 
  "end: " ++ show(end) ++ "\n" 
];
