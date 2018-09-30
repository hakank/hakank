% 
% Circular except 0 constraint in MiniZinc.
% 
% Cf http://hakank.org/minizinc/circular.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;

array[1..n] of var 0..5: x;


predicate next_val(var int: a, var int: b, int: v) =
   b = if (1+(a mod v)) == 0 then v else 1+(a mod v)  endif
;

predicate circular_except_0(array[int] of var int: t) =
  let {
    int: max_val = ub_array(t);
    int: t_len = length(t);
    int: n_states      = 6;
    int: input_max     = 6;
    int: initial_state = 1;
    set of int: accepting_states = 2..n_states;
    array[1..n_states, 1..input_max] of int: transition_fn =
      % 0 1 2 3 4 5
      [|1,3,4,5,6,2   % 1    init
       |2,3,0,0,0,0   % 2    1->0*->2 
       |3,0,4,0,0,0   % 3    2->0*->3
       |4,0,0,5,0,0   % 4    3->0*->4
       |5,0,0,0,6,0   % 5    4->0*->5
       |6,0,0,0,0,2   % 6    5->0*->1
      |];
    % regular cannot use 0 as a value, must start with 1.
    array[1..t_len] of var 1..n_states: y = [t[i]+1 | i in 1..t_len];
    % var 0..max_val: first_non_zero;
    % var 0..max_val: last_non_zero;
   }
    in
   regular(y, n_states, input_max, transition_fn, initial_state,
           accepting_states)

   % /\ % handle around the corner
   % next_val(last_non_zero, first_non_zero, max_val) 
   % (first_non_zero = 1+last_non_zero \/ (last_non_zero = 5 /\ first_non_zero = 1))
   % /\
   % exists(i,j in 1..t_len where i < j) (
   %  first_non_zero = t[i] /\
   %  last_non_zero = t[j] /\
   %  forall(k in 1..i-1) ( t[k] = 0)  /\
   %  forall(k in j+1..t_len) ( t[k] = 0) 
   % )
   % /\ % ensure full sequence, i.e. that the max value is in the sequence
   % exists(j in 1..t_len) (
   %      t[j] = max_val
   % )
   /\ % fix the number of 0s
   sum([t[i] = 0 | i in 1..t_len]) mod max_val = t_len mod max_val 
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


constraint 
  circular_except_0(x)
;



output [
  "x: \(x)\n",
];

