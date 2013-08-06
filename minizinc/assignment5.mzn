% 
% Assignment problem in MiniZinc.
% 
% 
% From
%  "SAS OR 9.1 User's Guide Mathematical Programming"
% """
% Consider assigning five programmers to five programming jobs. Each
% programmer prefers specific programming job over others. [...] 
% Suppose you ask each programmer to rank the jobs according to preference
% (using 1 for the most preferred job and 5 for the least preffered job).
% PROC ASSIGN maximizes the total preference of the group by minimizing the
% sum of the preferences. 
% 
%    PROGRAMMER     JOB1 JOB2 JOB3 JOB4 JOB5
%    PROGRAMMER1    4    1    3    5    2
%              2    2    1    3    4    5
%              3    3    2    4    1    5
%              4    2    3    4    5    1
%              5    4    2    3    1    5
% 
% """
% The solution should be:
% 
%   P1 -> 2
%   P2 -> 1
%   P3 -> 4
%   P4 -> 5
%   P5 -> 3
%
% And so it is:
% 
% summax: 8
% 0 1 0 0 0      2
% 1 0 0 0 0      1 
% 0 0 0 1 0      4
% 0 0 0 0 1      5
% 0 0 1 0 0      3
% 
% Which is a unique solution.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

% include "assignment_model.mzn";

int: rows;
int: cols;
array[1..rows, 1..cols] of var 0..1: x;
array[1..rows, 1..cols] of int: cost;
array[1..rows] of var 0..cols: jobs;
var int: summax;


%% From assignment_model.mzn
predicate assignment(array[int, int] of var 0..1: x, 
                     array[int, int] of int: cost, 
                     var int: summa
                 ) = 
   forall(i in index_set_1of2(x)) (
        sum(j in index_set_2of2(x)) (x[i,j]) = 1
    ) 
    /\ % more than one task per person?
   if card(index_set_1of2(x)) = card(index_set_2of2(x)) then
       forall(j in index_set_2of2(x)) (
           sum(i in index_set_1of2(x)) (x[i,j]) = 1
       )
   else  
       forall(j in index_set_2of2(x)) (
          sum(i in index_set_1of2(x)) (x[i,j]) <= 1
       )
   endif
   /\  
   summa = sum(i in index_set_1of2(x), j in index_set_2of2(x)) (
        x[i,j]*cost[i,j]
    )
;

%
% converts a binary matrix to a number array
% 
predicate binmatrix2num(array[int,int] of var int: x, array[int] of var int: nums) =

  forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
     nums[i] = j <-> x[i,j] = 1
  )

;

% solve satisfy;
solve :: int_search([x[i,j] | i in 1..rows, j in 1..cols] ++ jobs, first_fail, indomain_max, complete)  minimize summax;
% solve :: int_search([x[i,j] | i in 1..rows, j in 1..cols], first_fail, indomain_min, complete)  satisfy;


constraint
  assignment(x, cost, summax)
  % /\ summax <= 8
  /\
  binmatrix2num(x, jobs)
;

rows = 5;
cols = 5;

cost = array2d(1..rows, 1..cols, [
4, 1, 3, 5, 2,
2, 1, 3, 4, 5,
3, 2, 4, 1, 5,
2, 3, 4, 5, 1,
4, 2, 3, 1, 5]);


output 
[
  "\njobs: ", show(jobs), "\n",
  "summax: ", show(summax)
] ++ 
[
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in 1..rows, j in 1..cols
] ++ ["\n"];
