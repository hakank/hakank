% 
% Facility location problem in MiniZinc.
% 
% From 
% http://www.math.ohiou.edu/~vardges/math443/homeworks/homework1.doc
% """
% Problem 1. Facility Location problem.
% 
% A company is considering opening warehouses in 
%   four cities: New York, Los Angeles, Chicago, and Atlanta. 
% Each warehouse can ship 100 units per week. 
% The weekly fixed cost of keeping each warehouse open is 
%   $400 for New York, $500 for Los Angeles, $300 for Chicago, 
%   and $150 for Atlanta. 
%
% Region 1 of the country requires 80 units per week, 
% region 2 requires 70 units per week, and 
% region 3 requires 40 units per week. 
%
% The costs of sending 1 unit from a warehouse to a region 
% are shown in the following table:
%              	To
% from 	        Region 1  Region 2   Region 3
% New York 	$20 	  $40 	     $50
% Los Angeles 	$48 	  $15 	     $26
% Chicago 	$26 	  $35 	     $18
% Atlanta 	$24 	  $50 	     $35

% We wish to meet weekly demands at minimum cost, subject to the preceding information 
% and the following restrictions:
% 
%  1. If the New York warehouse is opened, then the Los Angeles warehouse 
%     must be opened.
%  2. At most three warehouses can be opened.
%  3. Either the Atlanta or the Los Angeles warehouse must be opened.
%
% Formulate an integer program that can be used to minimize the weekly costs 
% of meeting demands. 
 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: num_companies = 4;
int: num_regions = 3;
int: max_shipping = 100; % max shipping units per week per warehouse
int: new_york    = 1;
int: los_angeles = 2;
int: chicago     = 3;
int: atlanta     = 4;
array[1..num_companies] of int: fixed_cost = [400,500,300,150]; % dollars per week
array[1..num_regions] of int: demands = [80,70,40]; % units per week
array[1..num_companies, 1..num_regions] of int: costs = 
  array2d(1..num_companies, 1..num_regions, [
                          20, 40, 50,
                          48, 15, 26,
                          26, 35, 18,
                          24, 50, 35]);

% is this warehouse open?
array[1..num_companies] of var 0..1: open;
% what to ship to each region
array[1..num_companies, 1..num_regions] of var 0..max_shipping: ships;
% total cost
var 0..10000: tot_cost;

% solve satisfy;
% solve minimize tot_cost;
solve :: int_search(
        open ++ [ships[i,j] | i in 1..num_companies, j in 1..num_regions],
        smallest, % occurrence, 
        indomain_split, 
        complete) 
    % satisfy;
    minimize tot_cost;

constraint
      % each warehouse can only send max 100 units per week
      forall(i in 1..num_companies) (
        sum(j in 1..num_regions) (open[i]*ships[i,j]) <= max_shipping
      )

      /\ % the demands of the regions
      forall(j in 1..num_regions) (
         sum(i in 1..num_companies) (open[i]*ships[i,j]) >=  demands[j]
      )

      /\ % connect open with ships
      forall(i in 1..num_companies) (
          open[i] = 1 <-> sum(j in 1..num_regions) (ships[i,j]) > 0
      )
      /\ % total cost
      tot_cost = sum(i in 1..num_companies) (
         open[i]*(fixed_cost[i] + sum(j in 1..num_regions) (ships[i,j]*costs[i,j]))
      )
      /\ %  1. If the New York warehouse is opened, then the Los Angeles warehouse 
         %     must be opened.
      (open[new_york] = 1 -> open[los_angeles] = 1)

      /\ %  2. At most three warehouses can be opened.
      sum(open) <= 3
      
      /\ %  3. Either the Atlanta or the Los Angeles warehouse must be opened.
      (open[atlanta] = 1 \/ open[los_angeles] = 1)
     
      % /\ tot_cost <= 4570 % for solve satisfy
;

output
[
  "open: " ++ show(open) ++ "\n" ++ 
  "tot_cost: " ++ show(tot_cost) ++ "\n" ++
  "ships:"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(ships[i,j])
  | i in 1..num_companies, j in 1..num_regions
];
