% 
% Shopping with delivery cost in MiniZinc.
% 
% From https://groups.google.com/forum/embed/?place=forum/minizinc&showsearch=true&showpopout=true&showtabs=false&parenturl=http%3A%2F%2Fwww.minizinc.org%2Fforum.html#!topic/minizinc/pJaCxrVVzq8
% """
% Gabriel Fazzio de Paula
%
% Hello everyone!
%
% I'd like to start by saying I'm a complete noob in Minizinc. I've been battling with a problem 
% for a while now and a awesome reddit user mentioned Minizinc, and he/she said that it's the 
% perfect tool for the job and offered to help me, however, I had to give it a try by myself 
% first. Unfortunately, I was not able to solve it and too much time has past and I'm afraid 
% I'll never get that help again, so I'm here asking for help!
%
% The problems is: imagine I want to buy different kinds of parts from a set of different stores, 
% and each store could or couldn't have each specific part, and I want to minimize the cost of 
% buying all parts. The tricky part is that for each store I'd need to pay the shipping cost and 
% sometimes buying a part at the lower price, means buying it at a store that the shipping would 
% make it more expensive than buying it from a store that is more expensive but have other less 
% expensive parts. Here's some example data that might help
%
% XXXXXX	STORE_A	STORE_B	STORE_C			
% PART_A	N/A	2.25	2.9		STORE_A_DELIVERY_COST	12.56
% PART_B	N/A	3	N/A		STORE_B_DELIVERY_COST	15.2
% PART_C	2	15	7		STORE_C_DELIVERY_COST	33.5
%
% I'd like to find the minimum price to buy all parts including the delivery cost.
%
% What I've attempted to do (and probably is completely incorrect) is to make a decision variable 
% that would be either 1 or 0 for each Store,Part pair, and I'd minimize it for each Store, Part 
% pair, but I couldn't make it work.
%
% Thank you!
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_parts;
int: num_stores;

array[1..num_parts, 1..num_stores] of float: product_stores;
array[1..num_stores] of float: store_delivery_costs;

% decision variables
array[1..num_parts] of var 1..num_stores: x; % which store to by product p?

var 0.0..max(array1d(product_stores)): part_cost; 
var 0.0..max(store_delivery_costs): delivery_cost;
var float: total_cost = part_cost + delivery_cost;

solve minimize total_cost;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  total_cost >= 0.0 /\

  % a selected part must be in an available store
  forall(p in 1..num_parts) (
     % note: " ... > 0" don't work, must be greater than 0
     product_stores[p,x[p]] > 0.01
  )
  /\

  % total cost for the parts
  part_cost = sum([product_stores[p,x[p]] | p in 1..num_parts])
  /\

  % the delivery cost
  delivery_cost = sum([
                    % is there something bought from shop s?
                    if sum([x[p] = s | p in 1..num_parts]) > 0 then 
                        store_delivery_costs[s] 
                    else 
                        0 
                    endif                 
                | s in 1..num_stores])
;

output 
[
 "x: \(x)\n",
 "part_cost: \(part_cost)\n",
 "delivery_cost: \(delivery_cost)\n",
 "total_cost: \(total_cost)\n",
];

num_parts = 3;
num_stores = 3;

% product_stores[p,s]: price 0.0 means that p is _not_ available at store s
product_stores = 
array2d(1..num_parts,1..num_stores,
[
0.0,  2.25, 2.9,
0.0,  3.00, 0.0,
2.0, 15.00, 7.0,
]);

store_delivery_costs = [12.56, 15.2, 33.5];
