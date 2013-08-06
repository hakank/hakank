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
% This model is a MIP variant of 
%   http://www.hakank.org/minizinc/or_matching.mzn
%   http://www.hakank.org/minizinc/or_matching2.mzn
%
% and it use a 0/1 matrix for the assignments as well
% as using a dummy person which is assigned to the leftout.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 20;      % num persons
int: m = n div 2; % number of matchings

array[1..n, 1..n] of int: scores;
int: max_score = max([scores[i,j] | i, j in 1..n]);

% array[1..m] of var 0..max_score: s;

% decision variables: the assignment (matching)
array[1..n, 1..n] of var 0..1: x; 

% sum of assigned scores, to maximize
var 0..n*max_score: total;

% solve maximize total;
solve :: int_search(
        [x[i,j] | i, j in 1..n],
        first_fail,
        indomain_split, 
        complete) 
    maximize total;
% solve :: int_search(s, first_fail, indomain_max, complete) satisfy;

% The total score
constraint
  total = sum(i, j in 1..n where i < j) (
               x[i,j]*scores[i,j]
           )

;

constraint
   forall(i in 1..n) (
       % Just one matching per person
       sum([x[i,j] | j in 1..n]) = 1 /\
       sum([x[j,i] | j in 1..n]) = 1 /\

       % a person cannot be matched to him/herself
       x[i,i] = 0
   )
   /\
   forall(i,j in 1..n) (
       % ensure symmetry of matching:
       % a is assigned to b <-> b is assigned to a
       x[i,j] = x[j,i]
  )
;


% testing, for solve satisfy
% constraint total = 519;


% Note the scores are from 1.0 to 9.2 but since many solves don't
% support floats I multiplied by 10 so the range is from 10 to 92
% and 0 is for the diagonals.
% 
% #20 is the Dummy with score DD
%
int: DD = 00; % the score for the Dummy score
scores = array2d(1..n, 1..n, 
  [
%  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19, Dummy
  00, 30, 10, 33, 49, 50, 38, 32, 92, 61, 31, 22, 31, 55, 26, 19, 21, 61, 14, DD, %  1
  30, 00, 21, 23, 10, 33, 28, 15, 22, 22, 36, 33, 36, 38, 36, 24, 26, 22, 55, DD, %  2
  10, 21, 00, 10, 22, 26, 10, 22, 10, 41, 26, 52, 21, 24, 21, 33, 33, 10, 33, DD, %  3
  33, 23, 10, 00, 24, 25, 37, 24, 36, 36, 22, 22, 22, 22, 22, 10, 13, 36, 10, DD, %  4
  49, 10, 22, 24, 00, 41, 55, 42, 57, 57, 16, 15, 21, 69, 16, 33, 31, 57, 23, DD, %  5
  50, 33, 26, 25, 41, 00, 25, 19, 53, 53, 44, 33, 39, 61, 34, 27, 30, 53, 22, DD, %  6
  38, 28, 10, 37, 55, 25, 00, 29, 36, 36, 22, 22, 44, 58, 22, 32, 18, 36, 10, DD, %  7
  32, 15, 22, 24, 42, 19, 29, 00, 30, 30, 16, 10, 16, 21, 11, 59, 36, 61, 45, DD, %  8
  92, 22, 10, 36, 57, 53, 36, 30, 00, 69, 28, 27, 33, 50, 28, 21, 19, 69, 11, DD, %  9
  61, 22, 41, 36, 57, 53, 36, 30, 69, 00, 28, 27, 33, 50, 28, 21, 19, 69, 11, DD, % 10
  31, 36, 26, 22, 16, 44, 22, 16, 28, 28, 00, 33, 42, 39, 37, 30, 27, 28, 25, DD, % 11
  22, 33, 52, 22, 15, 33, 22, 10, 27, 27, 33, 00, 38, 36, 38, 26, 21, 27, 21, DD, % 12
  31, 36, 21, 22, 21, 39, 44, 16, 33, 33, 42, 38, 00, 39, 42, 57, 27, 33, 25, DD, % 13
  55, 38, 24, 22, 69, 61, 58, 21, 50, 50, 39, 36, 39, 00, 34, 27, 32, 50, 22, DD, % 14
  26, 36, 21, 22, 16, 34, 22, 11, 28, 28, 37, 38, 42, 34, 00, 30, 22, 28, 25, DD, % 15
  19, 24, 33, 10, 33, 27, 32, 59, 21, 21, 30, 26, 57, 27, 30, 00, 39, 52, 37, DD, % 16
  21, 26, 33, 13, 31, 30, 18, 36, 19, 19, 27, 21, 27, 32, 22, 39, 00, 19, 34, DD, % 17
  61, 22, 10, 36, 57, 53, 36, 61, 69, 69, 28, 27, 33, 50, 28, 52, 19, 00, 11, DD, % 18
  14, 55, 33, 10, 23, 22, 10, 45, 11, 11, 25, 21, 25, 22, 25, 37, 34, 11, 00, DD, % 19
  DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD, DD  % Dummy
]);


output [
  "total: " ++ show(total) ++ "\n" 
] ++ [
    if j = 1 then "\n" else " " endif ++
    show(x[i,j]) 
  | i,j in 1..n
] ++ 
[
  "\nAssignments:\n"
] ++ 
[
  if fix(x[i,j]) = 1 /\ i < j then 
     show(i) ++ " -> " ++ show(j) ++ ": " ++ show(scores[i,j]) ++ "\n"
  else "" endif
  | i,j in 1..n
] ++
[ "\nLeftout:" ] ++
[
  if fix(x[i, 20]) = 1 then show(i) else "" endif
  | i in 1..n
] ++
["\n"];