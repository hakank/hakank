% 
% Nonoram solver using regular and is written in all-MiniZinc.
% 
% This version uses the regular constraint with the following features:
%
%  * Compared to http://www.hakank.org/nonogram_regular.mzn
%    It calculated all the finite states given a Nonogram pattern,
%    instead of relying on an external program for doing this.
%
%  * Compared to http://www.hakank.org/nonogram_create_automaton.mzn
%    It calculates the states as par int (not var int), which
%    makes it possible to use some optimal regular constraints,
%    for example the one in Gecode/FlatZinc.
%
% Warning: the calculation of the states is quite ugly.
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

solve :: int_search(
     [x[i,j] | j in 1..cols, i in 1..rows], 
     first_fail, 
     indomain_min, 
     complete) 
satisfy;

%
% The approach is rather simple:
%  - zero_positions is a set of the positions in the state table where the 
%    state 0 should be, which also correspond to the state of the pattern "0"
%  - when this have been identified everything else comes to rest
%
% On the other hand, the calculation of the states is hairy, very hairy.
%
predicate make_automaton(array[int] of var int: x, array[int] of int: pattern) =
    let {
        int: n = length(pattern),
        % fix for "zero clues"
        int: len = max(length([pattern[i] | i in 1..n where pattern[i] > 0]) + sum(pattern),1),
        int: leading_zeros = sum(i in 1..n) (bool2int(pattern[i] = 0)),
        set of int: zero_positions = {sum(j in 1..i) (pattern[j]+1) -leading_zeros | i in 1..n where pattern[i] > 0},
       array[1..2*len] of 0..len*2: states = 
     if (length([pattern[i] | i in 1..n where pattern[i] > 0]) + sum(pattern)) = 0 then 
       [1,1]  % fix for "zero clues"
     else 
    [1, 2] ++
    [
       if i div 2 in zero_positions then
           if i mod 2 = 0 then
            0
           else
            (i div 2) + 1
           endif
       elseif (i-1) div 2 in zero_positions then
           if i mod 2 = 0 then
            (i div 2)+1
           else
            (i div 2)+2
           endif
       else
         if not( (((i-1) div 2) - 1) in zero_positions) then
            if i mod 2 = 0 then
               (i div 2) + 1
            else 
              if (i div 2) + 1 in zero_positions then
                  (i div 2) + 2
              else 
                  0
              endif
            endif
          else
             if i mod 2 = 0 then
                 (i div 2) + 1
             else 
                if not((i div 2) + 1 in zero_positions) then
                   0
                else 
                   (i div 2) + 2 
                endif
             endif
          endif
       endif
    | i in 3..2*(len-1)]
    ++
    [len, 0]
    endif
    } 
    in
    regular(
       x,
       len, 
       2, 
       array2d(1..len, 1..2, states),
       1, 
       {len}) % :: domain
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
     if fix(x[i,j]) = 1 then " " else "#" endif
    
  | i in 1..rows, j in 1..cols
] 
++ 
[
  "\n"
];
