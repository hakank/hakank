% 
% Matching problem of OR'ers in MiniZinc.
% 
% This problem was inspired by the matching of OR'ers by
% Figure It Out's "Cupids with Computers" (Capgemini blog)
% http://blogs.uk.capgemini.com/orblog/2011/02/25/cupids-with-computers/
% 
% where a bunch of OR'ers (including me) was matched according to some
% - not exactly stated - compatibility scores.
%
% Also, see the companion article
% "Two Deadly Sins"
% http://blogs.uk.capgemini.com/orblog/2011/03/02/two-deadly-sins/
%
% Here is an MiniZinc implementation of (pair) matching the different
% OR'ers. Note that the person unmatched person is also identified.
%
% I blogged about this model in
% "A matching problem, a related seating problem, and some other seating problems"
% http://www.hakank.org/constraint_programming_blog/2011/03/a_matching_problem_a_related_seating_problem_and_some_other_seating_pr_1.html
% 
% After some great suggestions by Sebastian Brand and Julien Fischer this is much 
% faster than the original version (I have updated the blog post about these improvements). 
% Thanks, guys!
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
% include "jacop.mzn";

int: n = 19;      % num persons
int: m = n div 2; % number of matchings

array[1..n, 1..n] of int: scores;
array[1..n*n, 1..3] of int: scores_table;


int: max_score = max([scores[i,j] | i, j in 1..n]);

% decision variables: the matchings
array[1..m, 1..2] of var 1..n: x; 

% the pair scores
% set of int: valid_scores = {scores[i,j] | i,j in 1..n};
% array[1..m] of var valid_scores: s; 
array[1..m] of var 0..max_score: s; 


% sum of s, to maximize
var 0..n*max_score: total = sum(s);

% The leftout. 0 means that there is not leftout
% Note: Now the leftout is shown just in the output.
% var 0..n: leftout;


% solve maximize total;
solve :: int_search(
        % for gecode 
        [x[i,j] | i in 1..m, j in 1..2],
        first_fail, % most_constrained, % first_fail
        indomain_split, % indomain_max, % indomain_median,
        complete) 
    maximize total;
    % satisfy;

constraint
  alldifferent([x[i,j] | i in 1..m, j in 1..2]) :: domain
;

constraint
  forall(i in 1..m) (
     s[i] = scores[x[i,1], x[i,2]] 

     % Yet another suggestion by Sebastian Brand:
     % using a table constraint:
     % table([x[i,1], x[i,2], s[i]], scores_table)

     % symmetry breaking
     % /\ x[i,1] <= x[i,2] 
     % Sebastian Brand spotted that it's enough with <
     % and this boosted many solvers.
     /\ x[i,1] < x[i,2] 
  )
;

% another symmetry breaking
constraint
  % decreasing(s) :: domain

  % after a suggestion by Sebastian:
  forall(i in 2..m)( x[i-1,1] < x[i,1] )
;

% show the leftout
% Note: The leftout is now just shown in the output.
% constraint
%    if n mod 2 != 0 then
%
%      % forall(i in 1..n) (     
%      %   leftout = i <-> not(exists(j in 1..m, k in 1..2) (x[j,k] = i))
%      % ) 
%
%      % This version was suggested by Sebastian Brand and 
%      % Julien Fischer.
%      leftout != 0 /\
%      forall(j in 1..m, k in 1..2) (
%         leftout != x[j,k]
%      )
%    else 
%      leftout = 0
%    endif
% ; 


% testing, for solve satisfy
% constraint total = 519;


% Note the scores are from 1.0 to 9.2 but since many solves don't
% support floats I multiplied by 10 so the range is from 10 to 92
% and 0 is for the diagonals.
scores = array2d(1..n, 1..n, 
  [
%  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
  00, 30, 10, 33, 49, 50, 38, 32, 92, 61, 31, 22, 31, 55, 26, 19, 21, 61, 14, %  1
  30, 00, 21, 23, 10, 33, 28, 15, 22, 22, 36, 33, 36, 38, 36, 24, 26, 22, 55, %  2
  10, 21, 00, 10, 22, 26, 10, 22, 10, 41, 26, 52, 21, 24, 21, 33, 33, 10, 33, %  3
  33, 23, 10, 00, 24, 25, 37, 24, 36, 36, 22, 22, 22, 22, 22, 10, 13, 36, 10, %  4
  49, 10, 22, 24, 00, 41, 55, 42, 57, 57, 16, 15, 21, 69, 16, 33, 31, 57, 23, %  5
  50, 33, 26, 25, 41, 00, 25, 19, 53, 53, 44, 33, 39, 61, 34, 27, 30, 53, 22, %  6
  38, 28, 10, 37, 55, 25, 00, 29, 36, 36, 22, 22, 44, 58, 22, 32, 18, 36, 10, %  7
  32, 15, 22, 24, 42, 19, 29, 00, 30, 30, 16, 10, 16, 21, 11, 59, 36, 61, 45, %  8
  92, 22, 10, 36, 57, 53, 36, 30, 00, 69, 28, 27, 33, 50, 28, 21, 19, 69, 11, %  9
  61, 22, 41, 36, 57, 53, 36, 30, 69, 00, 28, 27, 33, 50, 28, 21, 19, 69, 11, % 10
  31, 36, 26, 22, 16, 44, 22, 16, 28, 28, 00, 33, 42, 39, 37, 30, 27, 28, 25, % 11
  22, 33, 52, 22, 15, 33, 22, 10, 27, 27, 33, 00, 38, 36, 38, 26, 21, 27, 21, % 12
  31, 36, 21, 22, 21, 39, 44, 16, 33, 33, 42, 38, 00, 39, 42, 57, 27, 33, 25, % 13
  55, 38, 24, 22, 69, 61, 58, 21, 50, 50, 39, 36, 39, 00, 34, 27, 32, 50, 22, % 14
  26, 36, 21, 22, 16, 34, 22, 11, 28, 28, 37, 38, 42, 34, 00, 30, 22, 28, 25, % 15
  19, 24, 33, 10, 33, 27, 32, 59, 21, 21, 30, 26, 57, 27, 30, 00, 39, 52, 37, % 16
  21, 26, 33, 13, 31, 30, 18, 36, 19, 19, 27, 21, 27, 32, 22, 39, 00, 19, 34, % 17
  61, 22, 10, 36, 57, 53, 36, 61, 69, 69, 28, 27, 33, 50, 28, 52, 19, 00, 11, % 18
  14, 55, 33, 10, 23, 22, 10, 45, 11, 11, 25, 21, 25, 22, 25, 37, 34, 11, 00  % 19
]);


output [
  % from a suggestion from Sebastian: figure out the leftout 
  % in the output instead.
  let { 
     int: leftout = max(0..n diff { fix(x[j,k]) | j in 1..m, k in 1..2 }) 
    }
  in
  "max_score: " ++ show(max_score) ++ "\n" ++
  "total: " ++ show(total) ++ "\n" ++
  "leftout: " ++ show(leftout) ++ "\n" ++
  "p1 p2: score\n" ++
  "------------\n"
] ++ [
    show(x[i,1]) ++ " " ++ show(x[i,2]) ++ ": " ++ show(s[i]) ++ "\n"
  | i in 1..m

] ++ ["\n"];


% For the table/2 approach.
scores_table = array2d(1..n*n, 1..3,
  [
    1, 1, 00,
    1, 2, 30,
    1, 3, 10,
    1, 4, 33,
    1, 5, 49,
    1, 6, 50,
    1, 7, 38,
    1, 8, 32,
    1, 9, 92,
    1, 10, 61,
    1, 11, 31,
    1, 12, 22,
    1, 13, 31,
    1, 14, 55,
    1, 15, 26,
    1, 16, 19,
    1, 17, 21,
    1, 18, 61,
    1, 19, 14,
    2, 1, 30,
    2, 2, 00,
    2, 3, 21,
    2, 4, 23,
    2, 5, 10,
    2, 6, 33,
    2, 7, 28,
    2, 8, 15,
    2, 9, 22,
    2, 10, 22,
    2, 11, 36,
    2, 12, 33,
    2, 13, 36,
    2, 14, 38,
    2, 15, 36,
    2, 16, 24,
    2, 17, 26,
    2, 18, 22,
    2, 19, 55,
    3, 1, 10,
    3, 2, 21,
    3, 3, 00,
    3, 4, 10,
    3, 5, 22,
    3, 6, 26,
    3, 7, 10,
    3, 8, 22,
    3, 9, 10,
    3, 10, 41,
    3, 11, 26,
    3, 12, 52,
    3, 13, 21,
    3, 14, 24,
    3, 15, 21,
    3, 16, 33,
    3, 17, 33,
    3, 18, 10,
    3, 19, 33,
    4, 1, 33,
    4, 2, 23,
    4, 3, 10,
    4, 4, 00,
    4, 5, 24,
    4, 6, 25,
    4, 7, 37,
    4, 8, 24,
    4, 9, 36,
    4, 10, 36,
    4, 11, 22,
    4, 12, 22,
    4, 13, 22,
    4, 14, 22,
    4, 15, 22,
    4, 16, 10,
    4, 17, 13,
    4, 18, 36,
    4, 19, 10,
    5, 1, 49,
    5, 2, 10,
    5, 3, 22,
    5, 4, 24,
    5, 5, 00,
    5, 6, 41,
    5, 7, 55,
    5, 8, 42,
    5, 9, 57,
    5, 10, 57,
    5, 11, 16,
    5, 12, 15,
    5, 13, 21,
    5, 14, 69,
    5, 15, 16,
    5, 16, 33,
    5, 17, 31,
    5, 18, 57,
    5, 19, 23,
    6, 1, 50,
    6, 2, 33,
    6, 3, 26,
    6, 4, 25,
    6, 5, 41,
    6, 6, 00,
    6, 7, 25,
    6, 8, 19,
    6, 9, 53,
    6, 10, 53,
    6, 11, 44,
    6, 12, 33,
    6, 13, 39,
    6, 14, 61,
    6, 15, 34,
    6, 16, 27,
    6, 17, 30,
    6, 18, 53,
    6, 19, 22,
    7, 1, 38,
    7, 2, 28,
    7, 3, 10,
    7, 4, 37,
    7, 5, 55,
    7, 6, 25,
    7, 7, 00,
    7, 8, 29,
    7, 9, 36,
    7, 10, 36,
    7, 11, 22,
    7, 12, 22,
    7, 13, 44,
    7, 14, 58,
    7, 15, 22,
    7, 16, 32,
    7, 17, 18,
    7, 18, 36,
    7, 19, 10,
    8, 1, 32,
    8, 2, 15,
    8, 3, 22,
    8, 4, 24,
    8, 5, 42,
    8, 6, 19,
    8, 7, 29,
    8, 8, 00,
    8, 9, 30,
    8, 10, 30,
    8, 11, 16,
    8, 12, 10,
    8, 13, 16,
    8, 14, 21,
    8, 15, 11,
    8, 16, 59,
    8, 17, 36,
    8, 18, 61,
    8, 19, 45,
    9, 1, 92,
    9, 2, 22,
    9, 3, 10,
    9, 4, 36,
    9, 5, 57,
    9, 6, 53,
    9, 7, 36,
    9, 8, 30,
    9, 9, 00,
    9, 10, 69,
    9, 11, 28,
    9, 12, 27,
    9, 13, 33,
    9, 14, 50,
    9, 15, 28,
    9, 16, 21,
    9, 17, 19,
    9, 18, 69,
    9, 19, 11,
    10, 1, 61,
    10, 2, 22,
    10, 3, 41,
    10, 4, 36,
    10, 5, 57,
    10, 6, 53,
    10, 7, 36,
    10, 8, 30,
    10, 9, 69,
    10, 10, 00,
    10, 11, 28,
    10, 12, 27,
    10, 13, 33,
    10, 14, 50,
    10, 15, 28,
    10, 16, 21,
    10, 17, 19,
    10, 18, 69,
    10, 19, 11,
    11, 1, 31,
    11, 2, 36,
    11, 3, 26,
    11, 4, 22,
    11, 5, 16,
    11, 6, 44,
    11, 7, 22,
    11, 8, 16,
    11, 9, 28,
    11, 10, 28,
    11, 11, 00,
    11, 12, 33,
    11, 13, 42,
    11, 14, 39,
    11, 15, 37,
    11, 16, 30,
    11, 17, 27,
    11, 18, 28,
    11, 19, 25,
    12, 1, 22,
    12, 2, 33,
    12, 3, 52,
    12, 4, 22,
    12, 5, 15,
    12, 6, 33,
    12, 7, 22,
    12, 8, 10,
    12, 9, 27,
    12, 10, 27,
    12, 11, 33,
    12, 12, 00,
    12, 13, 38,
    12, 14, 36,
    12, 15, 38,
    12, 16, 26,
    12, 17, 21,
    12, 18, 27,
    12, 19, 21,
    13, 1, 31,
    13, 2, 36,
    13, 3, 21,
    13, 4, 22,
    13, 5, 21,
    13, 6, 39,
    13, 7, 44,
    13, 8, 16,
    13, 9, 33,
    13, 10, 33,
    13, 11, 42,
    13, 12, 38,
    13, 13, 00,
    13, 14, 39,
    13, 15, 42,
    13, 16, 57,
    13, 17, 27,
    13, 18, 33,
    13, 19, 25,
    14, 1, 55,
    14, 2, 38,
    14, 3, 24,
    14, 4, 22,
    14, 5, 69,
    14, 6, 61,
    14, 7, 58,
    14, 8, 21,
    14, 9, 50,
    14, 10, 50,
    14, 11, 39,
    14, 12, 36,
    14, 13, 39,
    14, 14, 00,
    14, 15, 34,
    14, 16, 27,
    14, 17, 32,
    14, 18, 50,
    14, 19, 22,
    15, 1, 26,
    15, 2, 36,
    15, 3, 21,
    15, 4, 22,
    15, 5, 16,
    15, 6, 34,
    15, 7, 22,
    15, 8, 11,
    15, 9, 28,
    15, 10, 28,
    15, 11, 37,
    15, 12, 38,
    15, 13, 42,
    15, 14, 34,
    15, 15, 00,
    15, 16, 30,
    15, 17, 22,
    15, 18, 28,
    15, 19, 25,
    16, 1, 19,
    16, 2, 24,
    16, 3, 33,
    16, 4, 10,
    16, 5, 33,
    16, 6, 27,
    16, 7, 32,
    16, 8, 59,
    16, 9, 21,
    16, 10, 21,
    16, 11, 30,
    16, 12, 26,
    16, 13, 57,
    16, 14, 27,
    16, 15, 30,
    16, 16, 00,
    16, 17, 39,
    16, 18, 52,
    16, 19, 37,
    17, 1, 21,
    17, 2, 26,
    17, 3, 33,
    17, 4, 13,
    17, 5, 31,
    17, 6, 30,
    17, 7, 18,
    17, 8, 36,
    17, 9, 19,
    17, 10, 19,
    17, 11, 27,
    17, 12, 21,
    17, 13, 27,
    17, 14, 32,
    17, 15, 22,
    17, 16, 39,
    17, 17, 00,
    17, 18, 19,
    17, 19, 34,
    18, 1, 61,
    18, 2, 22,
    18, 3, 10,
    18, 4, 36,
    18, 5, 57,
    18, 6, 53,
    18, 7, 36,
    18, 8, 61,
    18, 9, 69,
    18, 10, 69,
    18, 11, 28,
    18, 12, 27,
    18, 13, 33,
    18, 14, 50,
    18, 15, 28,
    18, 16, 52,
    18, 17, 19,
    18, 18, 00,
    18, 19, 11,
    19, 1, 14,
    19, 2, 55,
    19, 3, 33,
    19, 4, 10,
    19, 5, 23,
    19, 6, 22,
    19, 7, 10,
    19, 8, 45,
    19, 9, 11,
    19, 10, 11,
    19, 11, 25,
    19, 12, 21,
    19, 13, 25,
    19, 14, 22,
    19, 15, 25,
    19, 16, 37,
    19, 17, 34,
    19, 18, 11,
    19, 19, 00
  ]);

