% 
% Nonogram GCHQ puzzle Nonogram solver in MiniZinc.
%
% "A Christmas card with a cryptographic twist for charity"
%  http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx
%
% via 
%
% "Can you solve the British spy agency’s ridiculously difficult Christmas puzzle?"
% http://qz.com/570665/can-you-solve-the-british-spy-agencys-ridiculously-difficult-christmas-puzzle/
%
%
% This is not an ordinary Nonogram puzzle that can be solved with
% http://hakank.org/minizinc/nonogram_create_automaton2.mzn
% It has also some fixed black squares to be taken care of. Hence the separate model.
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

int: num_blacks;
array[int,int] of int: blacks;

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
       {len}) :: domain
;

constraint

  forall(j in 1..cols) (
    make_automaton([x[i,j] | i in 1..rows], [col_rules[j,k] | k in 1..col_rule_len])
  )
  /\
  forall(i in 1..rows) (
    make_automaton([x[i,j] | j in 1..cols], [row_rules[i,k] | k in 1..row_rule_len])
  )

  /\ % blacks
  forall(i in 1..num_blacks) (
    x[blacks[i,2],blacks[i,1]] = 2
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


rows = 25;
row_rule_len = 9;
row_rules = array2d(1..rows, 1..row_rule_len,
 [
 0,0,0,0,7,3,1,1,7,
 0,0,0,1,1,2,2,1,1,
 0,1,3,1,3,1,1,3,1,
 0,1,3,1,1,6,1,3,1,
 0,1,3,1,5,2,1,3,1,
 0,0,0,0,1,1,2,1,1,
 0,0,7,1,1,1,1,1,7,
 0,0,0,0,0,0,0,3,3,
 1,2,3,1,1,3,1,1,2,
 0,0,0,1,1,3,2,1,1,
 0,0,0,4,1,4,2,1,2,
 0,1,1,1,1,1,4,1,3,
 0,0,0,2,1,1,1,2,5,
 0,0,0,3,2,2,6,3,1,
 0,0,0,1,9,1,1,2,1,
 0,0,0,2,1,2,2,3,1,
 0,0,3,1,1,1,1,5,1,
 0,0,0,0,0,1,2,2,5,
 0,0,7,1,2,1,1,1,3,
 0,0,1,1,2,1,2,2,1,
 0,0,0,1,3,1,4,5,1,
 0,0,0,1,3,1,3,10,2,
 0,0,0,1,3,1,1,6,6,
 0,0,0,1,1,2,1,1,2,
 0,0,0,0,7,2,1,2,5

]);

cols = 25;
col_rule_len = 9;
col_rules = array2d(1..cols, 1..col_rule_len, 
  [
0,0,0,0,7,2,1,1,7,
0,0,0,1,1,2,2,1,1,
1,3,1,3,1,3,1,3,1,
0,1,3,1,1,5,1,3,1,
0,1,3,1,1,4,1,3,1,
0,0,0,1,1,1,2,1,1,
0,0,7,1,1,1,1,1,7,
0,0,0,0,0,0,1,1,3,
0,0,2,1,2,1,8,2,1,
0,2,2,1,2,1,1,1,2,
0,0,0,0,1,7,3,2,1,
0,1,2,3,1,1,1,1,1,
0,0,0,0,4,1,1,2,6,
0,0,3,3,1,1,1,3,1,
0,0,0,0,1,2,5,2,2,
2,2,1,1,1,1,1,2,1,
0,0,1,3,3,2,1,8,1,
0,0,0,0,0,0,6,2,1,
0,0,0,7,1,4,1,1,3,
0,0,0,0,1,1,1,1,4,
0,0,0,1,3,1,3,7,1,
1,3,1,1,1,2,1,1,4,
0,0,0,1,3,1,4,3,3,
0,0,1,1,2,2,2,6,1,
0,0,0,7,1,3,2,1,1,
   ]);


% Note: in [col,row] form
num_blacks = 22;
blacks = array2d(1..num_blacks, 1..2,
[
   4,4,
   5,4,
   13,4,
   14,4,
   22,4,
   7,9,
   8,9,
   11,9,
   15,9,
   16,9,
   19,9,
   7,17,
   12,17,
   17,17,
   21,17,
   4,22,
   5,22,
   10,22,
   11,22,
   16,22,
   21,22,
   22,22
 ]);

