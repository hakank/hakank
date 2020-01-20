/*
  Recover origin of pair differences in MiniZinc.

  From https://twitter.com/SturnioloSimone/status/1214588457135329280
  """
  Suppose there's a certain set of numbers - x_1, x_2, ..., x_n. You don't know n. 
  Suppose you only know certain linear combinations of them, L_ij = x_i-x_j, but you DON'T know 
  i or j. What's an algorithm to find as many of the xs as possible just from looking 
  at the Ls? Ideas?

  ... 
  (a follow up tweet:)
  There are four numbers. I can tell you that 1, 3, and 6 are differences between pairs of 
  numbers chosen among them (you don't know which pairs). Can you tell me what the numbers are?

  Something like that, except you don't know it's four.
  """ 

  For more about this problem see:
  - http://hakank.org/picat/Recover_origin_of_pair_differences.pdf
  - http://hakank.org/picat/linear_combinations.pi (Picat model)

  Also, see http://hakank.org/minizinc/linear_combinations.py, the Python program for doing
  experiments on this.

  This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/

include "globals.mzn"; 

int: num_diffs;
int: max_val = max(diffs)+1;
array[int] of int: diffs;

int: min_len = floor(1+sqrt(1+8*num_diffs)/2);

% string: allow_duplicates;
string: mode;

% decision variables
int: n; % length of solution
array[1..n] of var 1..max_val: x;

solve satisfy;
% solve :: int_search(x, , indomain_split, complete) satisfy;

constraint
  trace("diffs: \(diffs)\n") /\
  trace("min_len: \(min_len)\nmax_val: \(max_val)\n")

;

constraint
  all_different(x)
  /\
  increasing(x)
  /\
  x[1] = 1
  /\
  x[n] = max_val
  /\

  if mode = "first" then
    x[2]-x[1] >= x[n]-x[n-1]
  endif
  /\
  
  % From the observation that the sum of the differences of the solutions
  % is sum of the list of the differences + the first element in (the sorted list) diffs
  sum([x[i]-x[i-1] | i in 2..n]) = sum([diffs[i]-diffs[i-1] | i in 2..num_diffs]) + diffs[1]
  /\
  
  % experimental
  % if allow_duplicates = "experimental" then
  %    forall(t in 0..max_val) (
  %                             sum([abs(x[i]-x[j])=t | i in 1..n, j in i+1..n]) = count(diffs,t)
  %                             )
  %                              
  % endif
  % /\
 
  
  % All the differences from the pairs in the list x must be in the Diff list
  forall(i in 1..n, j in i+1..n) (
                                  exists(k in 1..num_diffs) (
                                                          abs(x[i]-x[j]) = diffs[k]
                                                          )
                                  )
  /\
  
  % Ensure that we cover all the differences (in diffs),
  % i.e. find some I and J (I<J) where abs(x[i]-x[j]) == d
  forall(d in diffs) (
                      % let {
                      %   var 1..n: i;
                      %   var 1..n: j;
                      % } in
                      % i < j /\
                      % d = abs(x[i]-x[j])
                      % This seems to be faster
                      exists(i in 1..n, j in i+1..n) (
                         d = abs(x[i]-x[j])
                      )
                   )
                 
;

output [
        "diffs: \(diffs)\n",
        "min_len: \(min_len)\nmax_val: \(max_val)\n",
        "x: \(x)\n"
];

%
% data
%
% allow_duplicates = "false";
% mode = "first";

% n = min_len;
% num_diffs = 13;
% diffs = [4,9,11,13,15,16,20,24,29,31,33,35,44];

% n = min_len;
% num_diffs = 19;
% diffs = [4, 7, 9, 11, 13, 15, 16, 18, 20, 22, 24, 29, 31, 33, 35, 38, 42, 44, 51];

% n = 11;
% num_diffs = 35;
% diffs = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,30,31,32,33,34,37,38,39];


% n = 20;
% num_diffs = 187;
% diffs = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190];
