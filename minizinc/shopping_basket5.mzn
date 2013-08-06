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

%
% Later note: I added an extra shop which don't have item 1,
% coded as a very large price (999999999).
% Another way is maybe to set the price to 0 and require that
% the prices to use is > 0.
%

% This is what Gecode/fz found after 25 seconds
% with largest, indomain_max
% total = 42013;
% x = array1d(1..29, [13, 20, 17, 18, 18, 13, 17, 17, 20, 13, 17, 17, 13, 13, 18, 17, 13, 13, 17, 8, 13, 36, 10, 17, 13, 13, 17, 20, 13]);


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
% Here is an alternative representation compared to
% http://www.hakank.org/minizinc/shopping_basket.mzn
%
% The elements in x represents the shop to buy the item from.
% This make the model slightly simpler.
array[1..num_items] of var 1..num_shops: x;

% total price
int: max_total = sum(i in 1..num_items) (max([costs[i,j] | j in 1..num_shops] ));
var 0..max_total: total; 

solve :: int_search(
         x,
         largest, % largest
         indomain_split, % indomain_max
         complete) 
        minimize total;
% solve minimize total;

% solve satisfy;

constraint

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
   % /\ total = 42013 % for solve satisfy
;

% just for the minizinc solver
output [
  "max_total: " ++ show(max_total) ++ "\n" ++
  "total: " ++ show(total) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 

];

%
% data
%
% % num_items = 2;
% % num_shops = 2;

% num_items = 3;
% num_shops = 3;

% % delivery_costs = [10,5];
% % delivery_costs_limits = [150,50];

% delivery_costs = [10,5,3];
% delivery_costs_limits = [150,50,100];



% % if a shop don't have an item, give it a very large price
% costs = array2d(1..num_items, 1..num_shops, [ 
%          % 100, 111,
%          %  90, 85
%          100, 111, 9999999,
%           90, 85, 2,
%           10, 10, 10
%          ]
%         );


% Data from http://pastie.org/958454
num_items = 29;
num_shops = 37;
% Coding N/A as 999999 
costs = array2d(1..num_items, 1..num_shops, [ 99999, 99999, 379, 99999, 99999, 99999, 99999, 99999, 99999, 369, 99999, 399, 345, 99999, 387, 499, 428, 99999, 394, 99999, 99999, 99999, 99999, 99999, 332, 99999, 99999, 424, 99999, 99999, 99999, 99999, 328, 99999, 409, 424, 99999,
 5974, 3190, 4999, 6295, 5310, 4923, 4178, 5642, 99999, 4532, 4532, 5227, 2921, 5974, 4789, 4790, 4563, 4806, 5362, 2921, 99999, 5360, 4840, 5642, 5707, 6027, 5171, 5642, 5310, 5304, 5241, 4195, 3195, 5873, 4295, 5443, 99999,
 99999, 3770, 3079, 99999, 3757, 3387, 3992, 3992, 4058, 3530, 3530, 3749, 3223, 4226, 3455, 3945, 3074, 3226, 3529, 3769, 3945, 3600, 3330, 3992, 3681, 3973, 3658, 3992, 3454, 3799, 3387, 3616, 3340, 99999, 3499, 3851, 99999,
 99999, 99999, 99999, 99999, 99999, 1431, 1522, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 1233, 99999, 99999, 99999, 1270, 1403, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 1432, 99999, 99999, 1522, 99999,
 99999, 2754, 99999, 99999, 3000, 2628, 99999, 99999, 99999, 2662, 2662, 99999, 99999, 99999, 99999, 99999, 2503, 2554, 99999, 2759, 99999, 2630, 2530, 3187, 2981, 99999, 2921, 3188, 2804, 2996, 99999, 99999, 3000, 99999, 99999, 3188, 99999,
 1085, 99999, 939, 99999, 99999, 1054, 1086, 99999, 99999, 917, 99999, 99999, 878, 99999, 99999, 99999, 1046, 99999, 99999, 99999, 99999, 950, 1033, 99999, 99999, 99999, 99999, 99999, 964, 99999, 99999, 964, 99999, 99999, 1270, 99999, 99999,
 1790, 1719, 1789, 2395, 2476, 1902, 1998, 1726, 2421, 1999, 1999, 2585, 1744, 2398, 1835, 1720, 1715, 2037, 1935, 2379, 2476, 2235, 1870, 2631, 2010, 2461, 2411, 2059, 1890, 2042, 2164, 2383, 1880, 2489, 1899, 2538, 2049,
 4248, 3790, 3564, 4154, 99999, 3560, 4012, 4012, 4248, 3548, 3548, 3765, 3240, 4248, 3767, 3995, 3202, 3514, 3617, 3789, 3776, 3615, 3500, 4012, 3800, 4341, 3677, 4012, 3690, 3967, 3648, 3634, 3918, 99999, 3410, 3870, 99999,
 99999, 2752, 2549, 99999, 99999, 2959, 2971, 99999, 99999, 2576, 2576, 99999, 99999, 99999, 99999, 99999, 99999, 2726, 99999, 2335, 99999, 2625, 2901, 99999, 99999, 99999, 2723, 2971, 99999, 99999, 99999, 2691, 2796, 99999, 99999, 2866, 99999,
 887, 747, 739, 99999, 99999, 706, 837, 837, 99999, 700, 700, 99999, 639, 99999, 837, 795, 746, 768, 713, 837, 99999, 715, 680, 837, 747, 894, 767, 837, 761, 787, 99999, 788, 788, 99999, 859, 837, 99999,
 2336, 1879, 1899, 99999, 2076, 1912, 1998, 2206, 99999, 1869, 1869, 2167, 1655, 2336, 1654, 2095, 1644, 1798, 1905, 1999, 99999, 1905, 1880, 2142, 1909, 2169, 2022, 2206, 2076, 2073, 1967, 1998, 2206, 99999, 2159, 2066, 99999,
 99999, 99999, 1749, 99999, 1916, 1981, 2036, 99999, 99999, 99999, 99999, 1862, 1606, 99999, 2036, 2395, 1606, 1708, 1758, 99999, 99999, 1760, 1942, 2036, 99999, 99999, 1866, 2036, 1875, 99999, 99999, 1916, 2036, 99999, 1824, 2036, 99999,
 1076, 838, 799, 99999, 99999, 901, 99999, 1017, 99999, 849, 849, 847, 730, 99999, 1017, 1017, 831, 933, 800, 772, 99999, 800, 883, 1017, 851, 1035, 932, 1017, 876, 956, 99999, 921, 957, 99999, 99999, 981, 99999,
 1552, 1282, 1239, 99999, 1379, 1226, 1465, 1465, 99999, 1224, 1224, 1295, 1096, 99999, 1465, 1465, 1222, 1188, 1223, 1279, 1379, 1225, 1205, 1465, 1305, 1508, 1343, 1465, 1340, 1377, 1271, 1327, 1465, 99999, 1359, 1414, 99999,
 1602, 1384, 1269, 99999, 99999, 1205, 99999, 1513, 99999, 1264, 1264, 1338, 1130, 1602, 1354, 99999, 1117, 1132, 1245, 1329, 99999, 1265, 1185, 1513, 1349, 1420, 1387, 1199, 99999, 99999, 1210, 99999, 1424, 99999, 99999, 1460, 99999,
 1687, 1393, 1339, 1649, 1499, 1231, 1593, 1593, 1698, 1331, 1331, 1408, 1191, 1612, 1593, 1595, 1176, 1291, 1329, 1389, 99999, 1330, 1210, 1593, 1345, 1542, 1460, 1593, 1456, 1497, 1273, 1443, 1499, 1457, 1454, 1537, 99999,
 2369, 2045, 1949, 2343, 2126, 1813, 1658, 2237, 2295, 1896, 1896, 2198, 1654, 2098, 1799, 2245, 1665, 1749, 1839, 1899, 2658, 1935, 1783, 2259, 2138, 2212, 2050, 1844, 2106, 2250, 1843, 2027, 1895, 2117, 1703, 2180, 1947,
 99999, 99999, 1999, 99999, 99999, 2285, 99999, 2601, 99999, 99999, 99999, 99999, 1908, 99999, 2093, 3060, 2256, 1970, 2028, 99999, 99999, 99999, 2241, 99999, 99999, 99999, 2384, 2601, 2163, 2445, 99999, 2448, 2448, 99999, 2259, 2601, 99999,
 3020, 2694, 2499, 2995, 2685, 2898, 99999, 2853, 99999, 2523, 2523, 2814, 2304, 99999, 2853, 2845, 2277, 2498, 2572, 2699, 99999, 2570, 2841, 2853, 3054, 2928, 2614, 2853, 2685, 2681, 2639, 2685, 2685, 2816, 3826, 99999, 99999,
 760, 558, 539, 99999, 595, 545, 288, 298, 670, 373, 373, 604, 397, 648, 371, 495, 388, 483, 461, 345, 695, 450, 525, 752, 600, 778, 740, 807, 391, 467, 552, 713, 389, 489, 389, 752, 499,
 891, 99999, 599, 99999, 99999, 584, 842, 841, 99999, 703, 99999, 631, 503, 99999, 842, 695, 605, 546, 595, 99999, 99999, 595, 562, 794, 535, 741, 771, 841, 635, 791, 99999, 792, 842, 990, 639, 794, 99999,
 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 927, 99999,
 99999, 99999, 99999, 99999, 99999, 2506, 99999, 99999, 99999, 2182, 99999, 99999, 99999, 99999, 99999, 99999, 2487, 99999, 2224, 99999, 99999, 2225, 2457, 2542, 99999, 99999, 2329, 99999, 2372, 99999, 99999, 99999, 2392, 99999, 2684, 2542, 99999,
 689, 526, 499, 673, 612, 483, 650, 650, 698, 543, 543, 532, 451, 689, 473, 645, 449, 450, 497, 529, 612, 500, 475, 650, 512, 641, 596, 650, 550, 611, 486, 589, 612, 550, 475, 627, 99999,
 99999, 99999, 439, 99999, 471, 99999, 99999, 99999, 99999, 419, 99999, 452, 397, 99999, 439, 579, 469, 99999, 446, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 415, 99999, 99999, 99999, 99999, 428, 99999, 499, 500, 99999,
 99999, 99999, 99999, 99999, 99999, 1474, 99999, 99999, 1845, 99999, 99999, 99999, 1246, 99999, 99999, 1845, 99999, 1439, 99999, 99999, 99999, 99999, 1446, 99999, 99999, 99999, 1437, 1568, 1396, 1474, 99999, 1476, 1531, 99999, 99999, 99999, 99999,
 2880, 2538, 2349, 2816, 2560, 2415, 2720, 2720, 99999, 2376, 2376, 2754, 2170, 2880, 2960, 2695, 2145, 2353, 2422, 2539, 3200, 2420, 2325, 2720, 2580, 2918, 2493, 2720, 2560, 2557, 2488, 2560, 2656, 99999, 2389, 2720, 99999,
 99999, 99999, 1269, 99999, 1432, 1190, 998, 1522, 1498, 1271, 1271, 1344, 1164, 1611, 899, 1395, 99999, 1168, 1239, 799, 99999, 1270, 1170, 99999, 99999, 99999, 1394, 1521, 790, 1430, 1216, 1790, 798, 1390, 864, 99999, 99999,
 817, 665, 618, 799, 726, 605, 772, 772, 99999, 645, 645, 643, 558, 99999, 772, 795, 618, 590, 596, 639, 99999, 600, 595, 772, 646, 798, 707, 99999, 665, 725, 634, 699, 726, 99999, 659, 745, 99999
] );

delivery_costs_limits = [ 6000, 6000, 5000, 2000, 3000, 7000, 5000, 2400, 4000, 2000, 1500, 3000, 7500, 1500, 1000, 4000, 5500, 4500, 5000, 3950, 4900, 5000, 7000, 5000, 4000, 5500, 5000, 5000, 3500, 2500, 3500, 3000, 6000, 2000, 5500, 5000, 2000 ];
delivery_costs = [ 395, 490, 395, 0, 400, 450, 398, 0, 300, 0, 0, 450, 395, 0, 0, 390, 450, 290, 390, 395, 390, 395, 395, 390, 450, 390, 350, 395, 345, 0, 395, 295, 395, 0, 395, 390, 399 ];
