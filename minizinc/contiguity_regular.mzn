% 
% Global constraint contiguity using regular in MiniZinc.
% 
% This is the contiguity constraint using regular constraint.
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
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 151;
array[1..n] of var 0..1: arr;


predicate contiguity(array[int] of var int: a) =
  let {
        int: n_states = 3,
        int: input_max = 2,
        int: initial_state = 1,
        set of int: accepting_states = {1,2,3},
        % the transition matrix
        array[1..n_states, 1..input_max] of int: transition_fn =
    array2d(1..n_states, 1..input_max,
    [ 
      % note:all states are accepting states
      1,2, % state 1 (start): input 0 -> state 1, input 1 -> state 2 i.e. 0*
      3,2, % state 2: 1* 
      3,0, % state 3: 0* 
    ]),
      int: len = length(a),
      % note: regular cannot handle 0 values, it must be > 0
      array[1..len] of var 1..2: reg_input
  } in
   forall(i in 1..len) (
     reg_input[i] = a[i] + 1
   )
   /\
   regular(reg_input, n_states, input_max, transition_fn,
                    initial_state, accepting_states)
;


solve satisfy;

constraint 
   contiguity(arr)
;

output [
   "arr : " ++ show(arr) ++ "\n" 

];
