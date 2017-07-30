% 
% Optimal picking one element from each list problem in MiniZinc.
% 
% From Stack Overflow
% Optimally picking one element from each list
% http://stackoverflow.com/questions/5645342/optimally-picking-one-element-from-each-list
% """
% I came across an old problem that you Mathematica/StackOverflow folks will 
% probably like and that seems valuable to have on StackOverflow for posterity.
% 
% Suppose you have a list of lists and you want to pick one element from each 
% and put them in a new list so that the number of elements that are identical to 
% their next neighbor is maximized. In other words, for the resulting list l, 
% minimize Length@Split[l]. In yet other words, we want the list with the fewest 
% interruptions of identical contiguous elements.
% 
% For example:
% 
% pick[{ {1,2,3}, {2,3}, {1}, {1,3,4}, {4,1} }]
%  --> {    2,      2,    1,     1,      1   }
% 
% (Or {3,3,1,1,1} is equally good.)
% 
% Here's a preposterously brute force solution:
% 
% pick[x_] := argMax[-Length@Split[#]&, Tuples[x]]
% 
% where argMax is as described here:
% posmax: like argmax but gives the position(s) of the element x for which f[x] is maximal
% 
% Can you come up with something better? The legendary Carl Woll nailed this for 
% me and I'll reveal his solution in a week.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: max_val = max([max(set2array(s[i])) | i in 1..n]);
array[1..n] of set of int: s;

% decision variables
array[1..n] of var 1..max_val: x;

var 0..max_val*n: sum_diffs; % to minimize


% solve satisfy;
solve minimize sum_diffs;
% solve :: int_search(
%         x, 
%        first_fail, 
%         indomain_split, 
%         complete) 
%     minimize sum_diffs;
%     % satisfy;

% Original problem
% constraint sum_diffs = 1;
% constraint sum_diffs = 16;


constraint
    % pick one element from each sub list
    forall(i in 1..n) (
        x[i] in s[i]
    )
    /\ % sum the number of differences between the selected elements
       % (to be minimized)
    sum_diffs = sum(i in 2..n) ( bool2int(x[i] != x[i-1]))
;

output [
   "max_val: " ++ show(max_val) ++ "\n" ++
   "sum_diffs: " ++ show(sum_diffs) ++ "\n" ++
   "x        : " ++ show(x) ++ "\n"
]
 ++ ["\n"]
;


%
% Problem instances
%

% Original problem
% n = 5;
% s = [
%     {1,2,3}, 
%     {2,3}, 
%     {1},
%     {1,3,4}, 
%     {4,1}
%     ];

% other problem instances (from the comments)

% There are 4 optimal solutions (sum_diffs = 2)
% n = 8;
% s = [
%      {3, 10, 6}, 
%      {8, 2, 10, 5, 9, 3, 6}, 
%      {3, 7, 10, 2, 8, 5, 9}, 
%      {6, 9, 1, 8, 3, 10}, 
%      {1}, 
%      {2, 9, 4}, 
%      {9, 5, 2, 6, 8, 7}, 
%      {6, 9, 4, 5}
%     ];
        
% There are 40 optimal solutions with sum_diffs = 5
% n = 8;
% s  = [{2, 8}, 
%       {4, 2}, 
%       {3}, 
%       {9, 4, 6, 8, 2}, 
%       {5}, 
%       {8, 10, 6, 2, 3}, 
%       {9, 4, 6, 3, 10, 1}, 
%       {9}];

% There are many many optimal solutions with sum_diffs = 16
n = 30;
s = [
    {4, 6, 18, 15}, 
    {1, 20, 16, 7, 14, 2, 9}, 
    {12, 3, 15}, 
    {17, 6, 13, 10, 3, 19}, 
    {1, 15, 2, 19}, 
    {5, 17, 3, 6, 14}, 
    {5, 17, 9}, 
    {15, 9, 19, 13, 8, 20}, 
    {18, 13, 5}, 
    {11, 5, 1, 12, 2}, 
    {10, 4, 7}, 
    {1, 2, 14, 9, 12, 3}, 
    {9, 5, 19, 8}, 
    {14, 1, 3, 4, 9}, 
    {11, 13, 5, 1}, 
    {16, 3, 7, 12, 14, 9}, 
    {7, 4, 17, 18, 6}, 
    {17, 19, 9}, 
    {7, 15, 3, 12}, 
    {19, 12, 5, 14, 8}, 
    {1, 10, 12, 8}, 
    {18, 16, 14, 19}, 
    {2, 7, 10}, 
    {19, 2, 5, 3}, 
    {16, 17, 3}, 
    {16, 2, 6, 20, 1, 3}, 
    {12, 18, 11, 19, 17}, 
    {12, 16, 9, 20, 4}, 
    {19, 20,10, 12, 9, 11}, 
    {10, 12, 6, 19, 17, 5}
  ];


% There are 80 solutions to this problem (sum_diffs = 10)
% n = 30;
% s = [{4, 2, 7, 5, 1, 9, 10}, 
%      {10, 1, 8, 3, 2, 7}, 
%      {9, 2, 7, 3, 6, 4,  5}, 
%      {10, 3, 6, 4, 8, 7}, 
%      {7}, 
%      {3, 1, 8, 2, 4, 7, 10, 6}, 
%      {7, 6}, 
%      {10, 2, 8, 5, 6, 9, 7, 3}, 
%      {1, 4, 8}, 
%      {5, 6, 1}, 
%      {3, 2, 1}, 
%      {10,6, 4}, 
%      {10, 7, 3}, 
%      {10, 2, 4}, 
%      {1, 3, 5, 9, 7, 4, 2, 8}, 
%      {7, 1, 3}, 
%      {5, 7, 1, 10, 2, 3, 6, 8}, 
%      {10, 8, 3, 6, 9, 4, 5, 7}, 
%      {3, 10, 5}, 
%      {1}, 
%      {7, 9, 1, 6, 2, 4}, 
%      {9, 7, 6, 2}, 
%      {5, 6, 9, 7}, 
%      {1, 5}, 
%      {1,9, 7, 5, 4}, 
%      {5, 4, 9, 3, 1, 7, 6, 8}, 
%      {6}, 
%      {10}, 
%      {6}, 
%      {7, 9}];

