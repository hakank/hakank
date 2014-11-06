% 
% Manasa and stones in MiniZinc.
% 
% From https://www.hackerrank.com/challenges/manasa-and-stones
% """
% Manasa is out on a hike with friends. She finds a trail of stones with numbers on them. 
% She starts following the trail and notices that two consecutive stones have a difference 
% of either a or b. Legend has it that there is a treasure trove at the end of the trail 
% and if Manasa can guess the value of the last stone, the treasure would be hers. 
% Given that the number on the first stone was 0, find all the possible values for the 
% number on the last stone.
%
% Note : The numbers on the stones are in increasing order
%
% Input Format
% The first line contains an integer T i.e. the number of Test cases. T testcases follow.
% Each testcase has 3 lines. The first line contains n ( the number of stones ) The second line contains a. The third line contains b.
%
% Output Format
% Space separated list of numbers which are the possible values of the last stone in increasing order.
%
% Constraints
% 1 <= T <= 10
% 1 <= n, a, b <= 103
%
% Sample Input 00
%
% 2
% 3 
% 1
% 2
% 4
% 10
% 100
%
% Sample Output 00
%
% 2 3 4 
% 30 120 210 300 

% Explanation
%
% All possible series for first test cases are given below.
%
%     0,1,_2_
%     0,1,_3_
%     0,2,_3_
%     0,2,_4_
%
% Hence the answer 2 3 4.
%
% Series with different number of final step for second test cases are

%     0, 10, 20, 30
%     0, 10, 20, 120
%     0, 10, 110, 120
%     0, 10, 110, 210
%     0, 100, 110, 120
%     0, 100, 110, 210
%     0, 100, 200, 210
%     0, 100, 200, 300

% hence the answer 30 120 210 300
% """
% 

%
% Note: This model don't handle the requirement of giving the _distinct_ final steps,
% this must be handled outside MiniZinc.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: a;
int: b;

% decision variables
array[1..n] of var 0..n*max(a,b): x;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

constraint
  x[1] = 0 /\
  forall(i in 2..n) (
     (x[i] = x[i-1] + a)
     \/
     (x[i] = x[i-1] + b)
  )

  /\ increasing(x) % redundant constraint
;

output [
  show(x[n])
];

% the two test examples
% n = 3;
% a = 1;
% b = 2;

n = 4;
a = 10;
b = 100;