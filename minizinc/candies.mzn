% 
% Candies in MiniZinc.
% 
% From HackerRank
% https://www.hackerrank.com/challenges/candies
% """
% Alice is a kindergarden teacher. She wants to give some candies to the 
% children in her class.  All the children sit in a line and each  of 
%  them  has a rating score according to his or her usual performance.  
% Alice wants to give at least 1 candy for each child.Children get 
% jealous of their immediate neighbors, so if two children sit next 
% to each other then the one with the higher rating must get more 
% candies. Alice wants to save money, so she wants to minimize the 
% total number of candies.
% 
% Input
% 
% The first line of the input is an integer N, the number of children 
% in Alice's class. Each of the following N lines contains an integer 
% indicates the rating of each child.
%
% Ouput
% 
% Output a single line containing the minimum number of candies Alice must give.
% 
% Sample Input
% 
% 3
% 1
% 2
% 2
% 
% Sample Ouput
% 
% 4
% 
% Explanation
% 
% The number of candies Alice must give are 1, 2 and 1.
% 
% Constraints:
%  
% N and the rating  of each child are no larger than 10^5.
% """

% There are 6 different solutions with the the minimum number of
% candies (4):
% 
% inp: [3, 1, 2, 2]
% x  : [3, 0, 1, 0]
% z  : 4
% ----------
% inp: [3, 1, 2, 2]
% x  : [2, 0, 2, 0]
% z  : 4
% ----------
% inp: [3, 1, 2, 2]
% x  : [1, 0, 3, 0]
% z  : 4
% ----------
% inp: [3, 1, 2, 2]
% x  : [2, 0, 1, 1]
% z  : 4
% ----------
% inp: [3, 1, 2, 2]
% x  : [1, 0, 2, 1]
% z  : 4
% ----------
% inp: [3, 1, 2, 2]
% x  : [1, 0, 1, 2]
% z  : 4


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of int: inp = [3,1,2,2];

% decision variables
array[1..n] of var 0..100000: x;
var int: z = sum(x);


%  solve minimize z;
solve satisfy;

% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   z  >= n /\
   forall(i in 2..n) (
     if inp[i-1] > inp[i] then
       x[i-1] > x[i]
     elseif inp[i-1] < inp[i] then
       x[i-1] < x[i]
     else
       true
     endif
   )

   /\ z = 4
;

output [
  "inp: " ++ show(inp) ++ "\n" ++
  "x  : " ++ show(x) ++ "\n" ++
  "z  : " ++ show(z) ++ "\n" 
];

