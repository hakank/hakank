% 
% Maximal non-segment sum in MiniZinc.
% 
% From  Alex Engelberg and Mark Engelberg "Solving Problems with Automata"
% https://www.youtube.com/watch?v=AEhULv4ruL4
% (around time 2:50, slide 5/40)
% """
% Given a sequence s of integers, find the maximum sum you can get by adding
% together a subsequence of numbers from s that do _not_ form a segment,
% i.e. a contigous block.
% """

% This version use an automata (see video)
%    State     0    1
%    --------------------------
%    q0        q0   q1  % start
%    q1        q2   q1
%    q2        q2   q3
%    q3        q3   q3
%    
% In MiniZinc representation: 
%    State     0    1
%    --------------------------
%    1         1    2  % start
%    2         3    2
%    3         3    4
%    4         4    4  % accepting state


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 5;
% array[1..n] of int: s = [-1,4,5,-3,-4];

int: n = 115;
array[1..n] of var int: s = [uniform(-20,20) | i in 1..n];

% decision variables
array[1..n] of var 0..1 : x;
var int: total = sum( i in 1..n ) (x[i]*s[i]);

%
% Decomposition of the contiguity constraint which ensure that
% all 1's in a is consecutive (a.k.a. "consecutive ones").
% It use the global constraint regular for this.
%
predicate non_contiguity(array[int] of var int: a) =
  let {
        int: n_states = 4,
        int: input_max = 2,
        int: initial_state = 1,
        set of int: accepting_states = {4},
        % the transition matrix
        array[1..n_states, 1..input_max] of int: transition_fn =
    array2d(1..n_states, 1..input_max,
    [ 
      1,2, % start state
      3,2, 
      3,4, 
      4,4, % only accepting state
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

% solve maximize total;
solve :: int_search(x,input_order, indomain_split,complete) maximize total;


constraint
  non_contiguity(x)
;

output [
  "total: \(total)\n",
  "s: \(s)\n",
  "x: \(x)\n",
  "selected: \([s[i] | i in 1..n where fix(x[i]) = 1])\n",
  "total: \(total)\n",
] 
;

