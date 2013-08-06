% 
% Create automaton for Nonorams in MiniZinc.
% 
% This model also creates the automaton and solves a
% Nonogram problem instance with regular constraint.
%
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: rows;
int: row_rule_len;
array[1..rows, 1..row_rule_len] of int: row_rules;
int: cols;
int: col_rule_len;
array[1..cols, 1..col_rule_len] of int: col_rules;

array[1..rows, 1..cols] of var 1..2: x;

% solve satisfy;
solve :: int_search(
        [x[i,j] | j in 1..cols, i in 1..rows], 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

%
% The approach is rather simple:
%  - zero_positions is a set of the positions in the state table where the state 0 
%    should be, which also correspond to the state of the pattern "0"
%  - when this have been identified everything else comes to rest
%
predicate make_automaton(array[int] of var int: x, array[int] of int: pattern) =
    let {
        int: n = length(pattern),
        int: len = length([pattern[i] | i in 1..n where pattern[i] > 0]) + sum(pattern),
        int: leading_zeros = sum(i in 1..n) (bool2int(pattern[i] = 0)),
        set of 1..len: zero_positions = {sum(j in 1..i) (pattern[j]+1) -leading_zeros | i in 1..n where pattern[i] > 0},
        array[1..len, 1..2] of var 0..len: states
    } 
    in
    states[1,1] = 1
    /\ 
    states[1,2] = 2
    /\

    forall(i in 2..len-1) (
       if i in zero_positions then
          states[i,1] = i+1 /\
          states[i,2] = 0 /\
          states[i+1,1] = i+1 /\
          states[i+1,2] = i+2
       else 
          if not(i - 1 in zero_positions) then
            states[i,1] = 0 /\
            states[i,2] = i+1
          else 
            true
          endif
       endif
    )
    /\
    states[len,1] = len
    /\
    states[len,2] = 0
    /\
    myregular(
       x,
       len, 
       2, 
       states,
       1, 
       {len})
;

constraint

    forall(j in 1..cols) (
      make_automaton([x[i,j] | i in 1..rows], [col_rules[j,k] | k in 1..col_rule_len])
    )
    /\
    forall(i in 1..rows) (
      make_automaton([x[i,j] | j in 1..cols], [row_rules[i,k] | k in 1..row_rule_len])
    )

;

output 
[
  if j = 1 then "\n" else "" endif ++
     show_cond(x[i,j] = 1, " ", "#") 
    
  | i in 1..rows, j in 1..cols
] 
++ 
[
  "\n"
];


%
% This is from lib/minizinc/std/regular.mzn
% though I changed it to allow for a variable d (the states).
%
predicate myregular(array[int] of var int: x, int: Q, int: S,
                  array[int,int] of var int: d, int: q0, set of int: F) =
    assert(Q > 0,
        "regular: 'Q' must be greater than zero",

    assert(S > 0,
        "regular: 'S' must be greater than zero",

    assert(index_set_1of2(d) = 1..Q /\ index_set_2of2(d) == 1..S,
        "regular: the transition function 'd' must be [1..Q,1..S]",

%    assert(forall([d[i, j] in 0..Q | i in 1..Q, j in 1..S]),
%        "regular: transition function 'd' points to states outside 0..Q",

        % Nb: we need the parentheses around the expression otherwise the
        % parser thinks it's a generator call!
    assert((q0 in 1..Q),
        "regular: start state 'q0' not in 1..Q",

    assert(F subset 1..Q,
        "regular: final states in 'F' contain states outside 1..Q",

        let {
            % If x has index set m..n-1, then a[m] holds the initial state
            % (q0), and a[i+1] holds the state we're in after  processing
            % x[i].  If a[n] is in F, then we succeed (ie. accept the string).
              int: m = min(index_set(x)),
              int: n = max(index_set(x)) + 1,
              array[m..n] of var 1..Q: a
            } in
        a[m] = q0 /\                    % Set a[0].
        forall(i in index_set(x)) (
            x[i] in 1..S /\             % Do this in case it's a var.
            a[i+1] = d[a[i], x[i]]      % Determine a[i+1].
        ) /\
        a[n] in F                       % Check the final state is in F.

     )))))
;

