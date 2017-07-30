% 
% Optimizing shopping basket in MiniZinc.
% 
% This is a slightly different model from
% http://www.hakank.org/minizinc/shopping_basket.mzn
% where the shop to buy from is represented in a single
% array.
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


% data defined below
int: num_items;
int: num_shops;
array[1..num_shops] of int: delivery_costs;
% assumes cost 0 if above the limit
array[1..num_shops] of int: delivery_costs_limits;
% array[1..num_items, 1..num_shops] of int: costs;
array[1..num_items, 1..num_shops] of int: costs;

%
% decision variables
%
% x: where to shop which item
% Here is an alternative representation compared to
% http://www.hakank.org/minizinc/shopping_basket.mzn
%
% The elements in x represents the shop to buy the item from.
% This make the model slightly simpler.
array[1..num_items] of var 1..num_shops: x;

array[1..num_shops] of var int: shop_costs;
array[1..num_shops] of var int: punish;
array[1..num_items] of var int: item_costs;

% total price
int: max_total = sum(i in 1..num_items) (max([costs[i,j] | j in 1..num_shops] ));
var 0..max_total: total; 

solve :: int_search(
         x,
         largest, % largest, % largest
         indomain_max, % indomain_split, % indomain_max
         complete) 
        minimize total;
        % satisfy;
% solve minimize total;

% solve satisfy;

constraint
   forall(i in 1..num_items) (
      costs[i,x[i]] > 0
   ) 
   /\
   total = sum(i in 1..num_items) (
       costs[i,x[i]]
   ) + 
   sum(j in 1..num_shops) (
       let {
         int: this_max = sum(i in 1..num_items) (max([costs[i,j] | i in 1..num_items])),
         var 0..this_max: this_cost = sum([costs[i,j]*bool2int(x[i]==j) | i in 1..num_items])
       } in
        delivery_costs[j]*bool2int(this_cost > 0 /\ this_cost < delivery_costs_limits[j])
   )
   /\
   forall(j in 1..num_shops) (
       let {
         int: this_max = sum(i in 1..num_items) (max([costs[i,j] | i in 1..num_items])),
         var 0..this_max: this_cost = sum([costs[i,j]*bool2int(x[i]==j) | i in 1..num_items])
       } in
        shop_costs[j] = this_cost /\
        punish[j] = delivery_costs[j]*bool2int(this_cost > 0 /\ this_cost < delivery_costs_limits[j])
   )
   /\
   forall(i in 1..num_items) (
      item_costs[i] = costs[i,x[i]]
   )
   % /\ total = 42013 % for solve satisfy

;

% just for the minizinc solver
output [
  % "max_total: " ++ show(max_total) ++ "\n" ++
  "total: " ++ show(total) ++ "\n" ++
  "punish: " ++ show(punish) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 

];

%
% data
%
%num_items = 2;
%num_shops = 2;

% num_items = 3;
% num_shops = 3;

% delivery_costs = [10,5];
% delivery_costs_limits = [150,50];

% delivery_costs = [10,5,3];
% delivery_costs_limits = [150,50,100];



% % if a shop don't have an item, give it a price of 0
% costs = array2d(1..num_items, 1..num_shops, [ 
%           %100, 111,
%           %90, 85
%         % shop1, shop2, shop3
%           100, 111,  0, % item1
%            90,  85,  2, % item2 
%            10,  10, 10  % item3
%           ]
%          );


% Data from http://pastie.org/958454
num_items = 29;
num_shops = 37;
% Coding N/A as 0
costs = array2d(1..num_items, 1..num_shops, [ 0, 0, 379, 0, 0, 0, 0, 0, 0, 369, 0, 399, 345, 0, 387, 499, 428, 0, 394, 0, 0, 0, 0, 0, 332, 0, 0, 424, 0, 0, 0, 0, 328, 0, 409, 424, 0,
 5974, 3190, 4999, 6295, 5310, 4923, 4178, 5642, 0, 4532, 4532, 5227, 2921, 5974, 4789, 4790, 4563, 4806, 5362, 2921, 0, 5360, 4840, 5642, 5707, 6027, 5171, 5642, 5310, 5304, 5241, 4195, 3195, 5873, 4295, 5443, 0,
 0, 3770, 3079, 0, 3757, 3387, 3992, 3992, 4058, 3530, 3530, 3749, 3223, 4226, 3455, 3945, 3074, 3226, 3529, 3769, 3945, 3600, 3330, 3992, 3681, 3973, 3658, 3992, 3454, 3799, 3387, 3616, 3340, 0, 3499, 3851, 0,
 0, 0, 0, 0, 0, 1431, 1522, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1233, 0, 0, 0, 1270, 1403, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1432, 0, 0, 1522, 0,
 0, 2754, 0, 0, 3000, 2628, 0, 0, 0, 2662, 2662, 0, 0, 0, 0, 0, 2503, 2554, 0, 2759, 0, 2630, 2530, 3187, 2981, 0, 2921, 3188, 2804, 2996, 0, 0, 3000, 0, 0, 3188, 0,
 1085, 0, 939, 0, 0, 1054, 1086, 0, 0, 917, 0, 0, 878, 0, 0, 0, 1046, 0, 0, 0, 0, 950, 1033, 0, 0, 0, 0, 0, 964, 0, 0, 964, 0, 0, 1270, 0, 0,
 1790, 1719, 1789, 2395, 2476, 1902, 1998, 1726, 2421, 1999, 1999, 2585, 1744, 2398, 1835, 1720, 1715, 2037, 1935, 2379, 2476, 2235, 1870, 2631, 2010, 2461, 2411, 2059, 1890, 2042, 2164, 2383, 1880, 2489, 1899, 2538, 2049,
 4248, 3790, 3564, 4154, 0, 3560, 4012, 4012, 4248, 3548, 3548, 3765, 3240, 4248, 3767, 3995, 3202, 3514, 3617, 3789, 3776, 3615, 3500, 4012, 3800, 4341, 3677, 4012, 3690, 3967, 3648, 3634, 3918, 0, 3410, 3870, 0,
 0, 2752, 2549, 0, 0, 2959, 2971, 0, 0, 2576, 2576, 0, 0, 0, 0, 0, 0, 2726, 0, 2335, 0, 2625, 2901, 0, 0, 0, 2723, 2971, 0, 0, 0, 2691, 2796, 0, 0, 2866, 0,
 887, 747, 739, 0, 0, 706, 837, 837, 0, 700, 700, 0, 639, 0, 837, 795, 746, 768, 713, 837, 0, 715, 680, 837, 747, 894, 767, 837, 761, 787, 0, 788, 788, 0, 859, 837, 0,
 2336, 1879, 1899, 0, 2076, 1912, 1998, 2206, 0, 1869, 1869, 2167, 1655, 2336, 1654, 2095, 1644, 1798, 1905, 1999, 0, 1905, 1880, 2142, 1909, 2169, 2022, 2206, 2076, 2073, 1967, 1998, 2206, 0, 2159, 2066, 0,
 0, 0, 1749, 0, 1916, 1981, 2036, 0, 0, 0, 0, 1862, 1606, 0, 2036, 2395, 1606, 1708, 1758, 0, 0, 1760, 1942, 2036, 0, 0, 1866, 2036, 1875, 0, 0, 1916, 2036, 0, 1824, 2036, 0,
 1076, 838, 799, 0, 0, 901, 0, 1017, 0, 849, 849, 847, 730, 0, 1017, 1017, 831, 933, 800, 772, 0, 800, 883, 1017, 851, 1035, 932, 1017, 876, 956, 0, 921, 957, 0, 0, 981, 0,
 1552, 1282, 1239, 0, 1379, 1226, 1465, 1465, 0, 1224, 1224, 1295, 1096, 0, 1465, 1465, 1222, 1188, 1223, 1279, 1379, 1225, 1205, 1465, 1305, 1508, 1343, 1465, 1340, 1377, 1271, 1327, 1465, 0, 1359, 1414, 0,
 1602, 1384, 1269, 0, 0, 1205, 0, 1513, 0, 1264, 1264, 1338, 1130, 1602, 1354, 0, 1117, 1132, 1245, 1329, 0, 1265, 1185, 1513, 1349, 1420, 1387, 1199, 0, 0, 1210, 0, 1424, 0, 0, 1460, 0,
 1687, 1393, 1339, 1649, 1499, 1231, 1593, 1593, 1698, 1331, 1331, 1408, 1191, 1612, 1593, 1595, 1176, 1291, 1329, 1389, 0, 1330, 1210, 1593, 1345, 1542, 1460, 1593, 1456, 1497, 1273, 1443, 1499, 1457, 1454, 1537, 0,
 2369, 2045, 1949, 2343, 2126, 1813, 1658, 2237, 2295, 1896, 1896, 2198, 1654, 2098, 1799, 2245, 1665, 1749, 1839, 1899, 2658, 1935, 1783, 2259, 2138, 2212, 2050, 1844, 2106, 2250, 1843, 2027, 1895, 2117, 1703, 2180, 1947,
 0, 0, 1999, 0, 0, 2285, 0, 2601, 0, 0, 0, 0, 1908, 0, 2093, 3060, 2256, 1970, 2028, 0, 0, 0, 2241, 0, 0, 0, 2384, 2601, 2163, 2445, 0, 2448, 2448, 0, 2259, 2601, 0,
 3020, 2694, 2499, 2995, 2685, 2898, 0, 2853, 0, 2523, 2523, 2814, 2304, 0, 2853, 2845, 2277, 2498, 2572, 2699, 0, 2570, 2841, 2853, 3054, 2928, 2614, 2853, 2685, 2681, 2639, 2685, 2685, 2816, 3826, 0, 0,
 760, 558, 539, 0, 595, 545, 288, 298, 670, 373, 373, 604, 397, 648, 371, 495, 388, 483, 461, 345, 695, 450, 525, 752, 600, 778, 740, 807, 391, 467, 552, 713, 389, 489, 389, 752, 499,
 891, 0, 599, 0, 0, 584, 842, 841, 0, 703, 0, 631, 503, 0, 842, 695, 605, 546, 595, 0, 0, 595, 562, 794, 535, 741, 771, 841, 635, 791, 0, 792, 842, 990, 639, 794, 0,
 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 927, 0,
 0, 0, 0, 0, 0, 2506, 0, 0, 0, 2182, 0, 0, 0, 0, 0, 0, 2487, 0, 2224, 0, 0, 2225, 2457, 2542, 0, 0, 2329, 0, 2372, 0, 0, 0, 2392, 0, 2684, 2542, 0,
 689, 526, 499, 673, 612, 483, 650, 650, 698, 543, 543, 532, 451, 689, 473, 645, 449, 450, 497, 529, 612, 500, 475, 650, 512, 641, 596, 650, 550, 611, 486, 589, 612, 550, 475, 627, 0,
 0, 0, 439, 0, 471, 0, 0, 0, 0, 419, 0, 452, 397, 0, 439, 579, 469, 0, 446, 0, 0, 0, 0, 0, 0, 0, 0, 415, 0, 0, 0, 0, 428, 0, 499, 500, 0,
 0, 0, 0, 0, 0, 1474, 0, 0, 1845, 0, 0, 0, 1246, 0, 0, 1845, 0, 1439, 0, 0, 0, 0, 1446, 0, 0, 0, 1437, 1568, 1396, 1474, 0, 1476, 1531, 0, 0, 0, 0,
 2880, 2538, 2349, 2816, 2560, 2415, 2720, 2720, 0, 2376, 2376, 2754, 2170, 2880, 2960, 2695, 2145, 2353, 2422, 2539, 3200, 2420, 2325, 2720, 2580, 2918, 2493, 2720, 2560, 2557, 2488, 2560, 2656, 0, 2389, 2720, 0,
 0, 0, 1269, 0, 1432, 1190, 998, 1522, 1498, 1271, 1271, 1344, 1164, 1611, 899, 1395, 0, 1168, 1239, 799, 0, 1270, 1170, 0, 0, 0, 1394, 1521, 790, 1430, 1216, 1790, 798, 1390, 864, 0, 0,
 817, 665, 618, 799, 726, 605, 772, 772, 0, 645, 645, 643, 558, 0, 772, 795, 618, 590, 596, 639, 0, 600, 595, 772, 646, 798, 707, 0, 665, 725, 634, 699, 726, 0, 659, 745, 0
] );

delivery_costs_limits = [ 6000, 6000, 5000, 2000, 3000, 7000, 5000, 2400, 4000, 2000, 1500, 3000, 7500, 1500, 1000, 4000, 5500, 4500, 5000, 3950, 4900, 5000, 7000, 5000, 4000, 5500, 5000, 5000, 3500, 2500, 3500, 3000, 6000, 2000, 5500, 5000, 2000 ];
delivery_costs = [ 395, 490, 395, 0, 400, 450, 398, 0, 300, 0, 0, 450, 395, 0, 0, 390, 450, 290, 390, 395, 390, 395, 395, 390, 450, 390, 350, 395, 345, 0, 395, 295, 395, 0, 395, 390, 399 ];
