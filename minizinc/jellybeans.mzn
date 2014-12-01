% 
% Jellybeans problem in MiniZinc.
% 
% http://l4f.cecs.anu.edu.au/puzzles/beginner/jellybeans
% """
% There are four children, Alice, Boris, Claire and David. 
% Their ages (not in order) are 6, 7, 8 and 9. 
% Each child has either 1, 2, 3 or 4 jellybeans (a different number each).
% 
%   1. Alice’s age plus her beans is the same as Boris’s age plus his beans.
%   2. Claire’s age is two more than the number of Alice’s beans.
%   3. Alice’s age is equal to the number of beans she and Claire have altogether.
% 
% How old is David, and how many beans has he?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;

int: Alice = 1;
int: Boris = 2;
int: Claire = 3;
int: David = 4;

set of int: age_set = {6,7,8,9};
set of int: bean_set = {1,2,3,4};

% decision variables

array[1..n] of var age_set: age;
array[1..n] of var bean_set: beans;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   all_different(age) /\
   all_different(beans) /\

   %   1. Alice’s age plus her beans is the same as Boris’s age plus his beans.
   age[Alice] + beans[Alice] = age[Boris] + beans[Boris] /\

   %   2. Claire’s age is two more than the number of Alice’s beans.
   age[Claire] = beans[Alice] + 2 /\

   %   3. Alice’s age is equal to the number of beans she and Claire have altogether.
   age[Alice] = beans[Alice] + beans[Claire]

;

output [
  "age  : ", show(age),"\n",
  "beans: ", show(beans),"\n",
  "David's age: ", show(age[David]), " David's beans: ", show(beans[David]), "\n",
];

