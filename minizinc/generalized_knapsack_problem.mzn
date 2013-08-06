% 
% Generalized Knapsack problem in MiniZinc.
% 
% From 
% http://www.math.ohiou.edu/~vardges/math443/homeworks/homework1.doc
% http://74.125.77.132/search?q=cache:gvXFEnS7aN4J:www.math.ohiou.edu/~vardges/math443/homeworks/homework1.doc+%22generalized+knapsack+problem%22&cd=36&hl=en&ct=clnk&gl=se
% """
% Problem 3. Generalized knapsack problem.
%
% Suppose that Tom is going on an overnight hike. There are 3 different items Tom is 
% considering taking along the trip. Tom's knapsack can hold up to 20 lbs of items. 
% The weight of item i is w[i]. Tom might take at most L[i] copies of item i.  
% The benefit Tom feels he would get from taking k copies of item i is b[i,k]. 
% The values of w[i], L[i] and b[i,k] are listed in the following table. 
%
% item weight(lbs) max # of copies Benefit
% i    w[i] L[i]                   b[i,1] b[i,2] b[i,3] b[i,4]
% 1    4    3                      45     70     90  
% 2    5    3                      20     40     50  
% 3    3    4                      50     70     85      95
%
% Formulate an integer program whose solution will tell Tom how to maximize the 
% total profit. 
%
% Hint: Note that b[i,k] is not a linear function on k, and that complicates the problem. 
% To overcome this difficulty, introduce the following decision variables. For each item i 
% (i=1,2,3) and possible number of copies k (k=1,...,L[i]): 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 3;
int: max_weight = 20;
int: max_limits = max(limits);
array[1..n] of int: weight = [4,5,3];
array[1..n] of int: limits = [3,3,4];
array[1..n, 1..max_limits] of 0..100: benefits = 
   array2d(1..n, 1..max_limits, 
      [
          45,70,90,0,
          20,40,50,0,
          50,70,85,95
      ]);

array[1..n] of var 1..max_limits: x;
var int: tot_benefit;
var int: tot_weight;


predicate generalized_knapsack(array[int] of int: weight, 
                               array[int] of int: limits, 
                               array[int,int] of int: benefits, 
                               var int: tot_weight, 
                               var int: tot_benefit, 
                               array[int] of var int: x) =
   % total benefits
   tot_benefit = sum(i in 1..n) (benefits[i,x[i]]) 
   /\ % satisfy the limits for each item
   forall(i in 1..n) (
      x[i] <= limits[i]
   )
   /\ % total weight
   tot_weight = sum(i in 1..n) ( x[i]*weight[i] ) 
   /\
   tot_weight <= max_weight

;

% solve satisfy;
solve :: int_search(
        [tot_benefit,tot_weight] ++ x, 
        first_fail, 
        indomain_max, 
        complete) 
    maximize tot_benefit;
    % satisfy;

constraint
   generalized_knapsack(weight, limits, benefits, tot_weight, tot_benefit, x)
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "tot_benefit: " ++ show(tot_benefit) ++ "\n" ++  
  "tot_weight: " ++ show(tot_weight) ++ "\n" 

];
