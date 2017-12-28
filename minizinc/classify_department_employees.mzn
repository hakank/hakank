% 
% Classify Department Employees in MiniZinc.
% 
% From https://dmcommunity.org/challenge/challenge-sep-2017/
% """
% A human resource office has information about all employees in every department including: 
% salary, marital status, age, etc. Help the office to create a decision model that for 
% each department calculates minimal, maximal, and average salaries along with a number of 
% high-paid employees using rules like "Salary > 85000"
%
% Name     Marital Status  Gender    Age    Salary
% ------------------------------------------------
% Robinson Married         Female    25      20000
% Warner   Married         Male      45     150000
% Stevens  Single          Male      24      35000
% White    Married         Female    32      75000
% Smith    Single          Male      46     110000
% Green    Married         Female    28      40000
% Brown    Married         Male      32      65000
% Klaus    Married         Male      54      85000
% Houston  Single          Female    47      35000
% Long     Married         Male      29      40000
% Short    Single          Male      22      20000
% Doe      Single          Female    21      21000
% """

% Note: The statement mentions department but there is no department info, so I skip this.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 12; 

enum marital = {Married,Single};
enum gender = {Female,Male};
enum name = {Robinson,Warner,Stevens,White,Smith,Green,Brown,Klaus,Houston,Long,Short,Doe};

array[1..n,1..5] of int: people = 
array2d(name,1..5, 
[
Robinson,Married,Female,25, 20000,
Warner  ,Married,Male  ,45,150000,
Stevens ,Single ,Male  ,24, 35000,
White   ,Married,Female,32, 75000,
Smith   ,Single ,Male  ,46,110000,
Green   ,Married,Female,28, 40000,
Brown   ,Married,Male  ,32, 65000,
Klaus   ,Married,Male  ,54, 85000,
Houston ,Single ,Female,47, 35000,
Long    ,Married,Male  ,29, 40000,
Short   ,Single ,Male  ,22, 20000,
Doe     ,Single ,Female,21, 21000,
]);

% decision variables

% salaries statistics
var int: max_salaries = max([people[p,5] | p in name]);
var int: min_salaries = min([people[p,5] | p in name]);
var int: avg_salaries = sum([people[p,5] | p in name]) div n ;

% high paid
var set of name: high_paid = { p | p in name where people[p,5] > 85000 };
var int: num_high_paid = card(high_paid);

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  true
;

output 
[
 "min: \(min_salaries)\n",
 "avg: \(avg_salaries)\n",
 "max: \(max_salaries)\n\n",
 "num_high_paid: \(num_high_paid) (\(high_paid))\n",

 % extras
 "low paid: \([p | p in name where people[p,5] <= 30000])\n",

 "mean age: \(sum([people[p,4] | p in name]) div n )\n"
];

