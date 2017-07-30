% 
% Kidney exchange in MiniZinc.
% 
% A simple model of kidney exchange, inspired by Pascal Van Hentenryck's
% introduction of Discrete Optimization.
%
% The objective is to maximize the number of exchanges.
%
% Note that we are looking for cycles, or at least that anyone
% that give a kidney also receive a kidney.
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_people;
array[1..num_people] of set of int: compatible;

% decision variables
% who get who's kidney
array[1..num_people] of var 0..num_people: x;

var 0..num_people: z = sum([bool2int(x[i] > 0) | i in 1..num_people]);

% solve satisfy;
% solve maximize z;
solve :: int_search(x, first_fail, indomain_min, complete) maximize z;

constraint
   alldifferent_except_0(x)
   /\
   forall(i in 1..num_people) (
     (x[i] = 0)
     \/
     (
        x[i] in compatible[i] /\
        x[x[i]] > 0 % the giver must also receive a kidney
     )
   )
;

% constraint z = 6; % for solve satisfy of Pascal's example

output [
  "z: " ++ show(z) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
];


%
% data
%

% The compatibility matrix 
% (from Pascal's introduction lecture)
% who can give a kidney to person p
% This is a directed graph
num_people = 8;
compatible = 
  [
    {2,3}, % 1
    {1,6}, % 2
    {1,4,7}, % 3
    {2}, % 4  
    {2}, % 5
    {5}, % 6
    {8}, % 7
    {3}, % 8
  ];
  


% Example from Al Roth's "CRCS Lunch Seminar" (YouTube)
% about 23 min into the lecture.
%
% num_people = 13;
% % who can give a kidney to person p
% compatible = 
%   [
%     {}, % 1
%     {1,4,5}, % 2
%     {2}, % 3
%     {3,9}, % 4  
%     {6,8}, % 5
%     {7}, % 6
%     {}, % 7
%     {}, % 8
%     {10}, % 9
%     {}, % 10
%     {13}, % 11
%     {11}, % 12
%     {12}, % 13
%   ];
