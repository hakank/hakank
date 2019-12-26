% 
% Nonoram solver using regular and regular expressions all-MiniZinc.
% 
% This version uses the regular constraint with the following features:
%
%  * It calculated all the finite states given a Nonogram pattern,
%    using the global constraint regular(x, s) where s is a regular
%    expression string.
%
% See an example below how to the regexps look like.
%
% Compare with http://hakank.org/minizinc/nonogram_create_automaton2.mzn
% which is much hairier.
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
% * Convert a pattern the 1s to a regular expression string (regexp)
% * Use this regexp in the regular/2 global constraint.
% Note that we are using 1 and 2 instead of 0 and 1:
%   0 -> 1
%   1 -> 2
%
predicate make_regexp(array[int] of var int: x, array[int] of int: pattern) =
  let {
  int: len = length(pattern);
  string: s = "1*" ++ concat([ "2{" ++ show(pattern[i]) ++ "}" ++ if i < len then "1+" else "" endif | i in 1..len where pattern[i] > 0]) ++ "1*";
  } in
  % trace("pattern: \(pattern)  s: \(s)\n") /\
  regular(x, s)

;

constraint
      forall(j in 1..cols) (
        make_regexp([x[i,j] | i in 1..rows], [col_rules[j,k] | k in 1..col_rule_len])
      )
      /\
      forall(i in 1..rows) (
        make_regexp([x[i,j] | j in 1..cols], [row_rules[i,k] | k in 1..row_rule_len])
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


%% A sample problem instance and (hand coded) regexps
%%
% %% 
% %% ECLiPSe 
% %% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
% %% Problem ps
% rows = 9;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len,
%         [0,3, % 1*2{3}0*
%          2,1, % 1*2{2}0+2{1}0*
%          3,2, % 1*2{3}0+2{2}0*
%          2,2, % 1*2{2}0+2{2}0* 
%          0,6, % 1*2{6}0*
%          1,5, % 1*2{1}0+2{5}0*
%          0,6, % 1*2{6}0*
%          0,1, % 1*2{1}0*
%          0,2  % 1*2{2}0*
%          ]);

% cols = 8;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%      [1,2, % 1*2{2}1+2{2}1*
%       3,1, % 1*2{3}1+2{1}1*
%       1,5, % 1*2{1}1+2{5}1*
%       7,1, % 1*2{7}1+2{1}1*
%       0,5, % 1*2{5}1*
%       0,3, % 1*2{3}1*
%       0,4, % 1*2{4}1*
%       0,3  % 1*2{3}1*   
%   ]);
