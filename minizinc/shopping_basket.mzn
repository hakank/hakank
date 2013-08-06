% 
% Optimizing shopping basket in MiniZinc.
% 
% From 
% http://stackoverflow.com/questions/2822082/how-to-use-constraint-programming-for-optimizing-shopping-baskets
% """
% How to use constraint programming for optimizing shopping baskets?
% 
% I have a list of items I want to buy. The items are offered by different 
% shops and different prices. The shops have individual delivery costs. 
% I'm looking for an optimal shopping strategy (and a java library 
% supporting it) to purchase all of the items with a minimal total price.
% 
% Example:
% 
%    * Item1 is offered at Shop1 for $100, at Shop2 for $111.
%    * Item2 is offered at Shop1 for $90, at Shop2 for $85.
%    * Delivery cost of Shop1: $10 if total order < $150; $0 otherwise
%    * Delivery cost of Shop2: $5 if total order < $50; $0 otherwise 
%
%    * If I buy Item1 and Item2 at Shop1 the total cost 
%      is $100 + $90 +$0 = $190.
%    * If I buy Item1 and Item2 at Shop2 the total cost 
%      is $111 + $85 +$0 = $196.
%    * If I buy Item1 at Shop1 and Item2 at Shop2 the total cost 
%      is $100 + $10 + $85 + $0 = 195. 
%
% I get the minimal price if I order Item1 and Item2 at Shop1: $190
% What I tried so far
% 
% I asked another question before that led me to the field of constraint 
% programming. I had a look at cream and choco, but I did not figure 
% out how to create a model to solve my problem.
%
%          | shop1 | shop2 | shop3 | ...
% -----------------------------------------
% item1    | p11   | p12   | p13   |
% item2    | p21   | p22   | p23   |
%  .       |       |       |       |
%  .       |       |       |       |
% -----------------------------------------
% shipping | s1    | s2    | s3    |
% limit    | l1    | l2    | l3    |
% -----------------------------------------
% total    | t1    | t2    | t3    |
% -----------------------------------------
%
% My idea was to define these constraints:
%
%  * each price "p xy" is defined in the domain (0, c) where c is 
%    the price of the item in this shop
%  * only one price in a line should be non zero
%  * if one or more items are bought from one shop and the sum of 
%    the prices is lower than limit, then add shipping cost to the total cost
%  * shop total cost is the sum of the prices of all items in a shop
%  * total cost is the sum of all shop totals
%
% The objective is "total cost". I want to minimize this.
%
% In cream I wasn't able to express the "if then" constraint for conditional 
% shipping costs.
% 
% In choco these constraints exist, but even for 5 items and 10 shops 
% the program was running for 10 minutes without finding a solution.
%
% Question
%
% How should I express my constraints to make this problem solvable 
% for a constraint programming solver?
% """
% 
% Note: This is not what was asked for, but since I'm so lazy I
% modelled it in MiniZinc instead.
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Later note: I added an extra shop which don't have item 1,
% coded as a very large price (999999999).
% Another way is maybe to set the price to 0 and require that
% the prices to use is > 0.
%

% data defined below
int: num_items;
int: num_shops;
array[1..num_shops] of int: delivery_costs;
% assumes cost 0 if above the limit
array[1..num_shops] of int: delivery_costs_limits;
array[1..num_items, 1..num_shops] of int: costs;

%
% decision variables
%
% x: where to shop which item
% (alternative representation: one array with the domain of 1..num_shops)
array[1..num_items,1..num_shops] of var 0..1: x;

% total price
var int: total; 

solve minimize total;
% solve satisfy;

constraint

   % we must buy all items (from some shop)
   forall(i in 1..num_items) (
       sum([x[i,j] | j in 1..num_shops]) = 1
   )
   /\ 
   total = sum(i in 1..num_items) (
       % costs of the products
       sum(j in 1..num_shops) (
          x[i,j]*costs[i,j]
       )
   ) + sum(j in 1..num_shops) (
       %  and delivery costs if total for a shop < limit
       let {
         var int: this_cost = sum([costs[i,j]*x[i,j] | i in 1..num_items])
       } in
       delivery_costs[j]*bool2int(this_cost > 0 /\ this_cost < delivery_costs_limits[j])
   )
;


%
% data
%
% num_items = 2;
% num_shops = 2;

num_items = 3;
num_shops = 3;

% delivery_costs = [10,5];
% delivery_costs_limits = [150,50];

delivery_costs = [10,5,3];
delivery_costs_limits = [150,50,100];



% if a shop don't have an item, give it a very large price
costs = array2d(1..num_items, 1..num_shops, [ 
         % 100, 111,
         %  90, 85
         100, 111, 9999999,
          90, 85, 2,
          10, 10, 10
         ]
        );


output [
  "total: " ++ show(total) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 

];
