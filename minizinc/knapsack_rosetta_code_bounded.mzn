% 
% Knapsack (Bounded) in MiniZinc.
% 
% http://rosettacode.org/wiki/Knapsack_problem/Bounded
% """
% A tourist wants to make a good trip at the weekend with his 
% friends. They will go to the mountains to see the wonders of 
% nature. So he needs some items during the trip. Food, clothing, 
% etc. He has a good knapsack for carrying the things, but he knows 
% that he can carry only 4 kg weight in his knapsack, because they 
% will make the trip from morning to evening. He creates a list of 
% what he wants to bring for the trip, but the total weight of all 
% items is too much. He adds a value to each item. The value represents 
% how important the thing for the tourist. The list contains which 
% items are the wanted things for the trip, what is the weight and 
% value of an item, and how many units does he have from each items.
%
% This is the list:
% Table of potential knapsack items item 	weight (dag) (each) 	value (each) 	piece(s)
% map 	9 	150 	1
% compass 	13 	35 	1
% water 	153 	200 	2
% sandwich 	50 	60 	2
% glucose 	15 	60 	2
% tin 	68 	45 	3
% banana 	27 	60 	3
% apple 	39 	40 	3
% cheese 	23 	30 	1
% beer 	52 	10 	3
% suntan cream 	11 	70 	1
% camera 	32 	30 	1
% T-shirt 	24 	15 	2
% trousers 	48 	10 	2
% umbrella 	73 	40 	1
% waterproof trousers 	42 	70 	1
% waterproof overclothes 	43 	75 	1
% note-case 	22 	80 	1
% sunglasses 	7 	20 	1
% towel 	18 	12 	2
% socks 	4 	50 	1
% book 	30 	10 	2
% knapsack 	<=400 dag 	 ? 	 ? 
%
%
% The tourist can choose to take any combination of items from the 
% list, and some number of each item is available (see the column 
% "Piece(s)" of the list!). He may not cut the items, so he can only 
% take whole units of any item.
% 
% Which items does the tourist carry in his knapsack so that their 
% total weight does not exceed 4 kg, and their total value is maximised?
% """

% (Note: 4kg is 400dag)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: num_items = 22;
array[1..num_items, 1..3] of int: items;
array[1..num_items] of string: items_str;

int: max_num_items = max([items[i,3] | i in 1..num_items]);

% decision variables
array[1..num_items] of var 0..max_num_items: x;
var int: total_value  = sum(i in 1..num_items) ( x[i]*items[i,2] );
var int: total_weight = sum(i in 1..num_items) ( x[i]*items[i,1] );

solve :: int_search(
        x, 
        anti_first_fail, 
        indomain_max, 
        complete) 
    maximize total_value;
    % satisfy;


constraint
    total_weight <= 400 

    /\ % don't exceed the available pieces
    forall(i in 1..num_items) (
       x[i] <= items[i,3]
    )

    % /\ total_value = 1010 % testing all optimal solutions
;




output 
[
  "total_value: " ++ show(total_value) ++ "\n" ++ 
  "total_weight: " ++ show(total_weight) ++ "\n"
] ++
[
   if fix(x[i]) > 0 then 
     show(items_str[i]) ++ ": " ++ show(x[i]) ++ "\n"
   else 
     ""
   endif
   | i in 1..num_items

]
 ++ ["\n"]
;


%
% Data
%

items_str = 
["map","compass","water","sandwich","glucose","tin","banana","apple","cheese",
"beer","suntancream","camera","T-shirt","trousers","umbrella","w-trousers",
"w-overclothes","note-case","sunglasses","towel","socks","book"];

%
% weight (dag) (each)  value (each)  pieces(s)
%
items = array2d(1..num_items, 1..3, [
9,	150,	1,
13,	35,	1,
153,	200,	2,
50,	60,	2,
15,	60,	2,
68,	45,	3,
27,	60,	3,
39,	40,	3,
23,	30,	1,
52,	10,	3,
11,	70,	1,
32,	30,	1,
24,	15,	2,
48,	10,	2,
73,	40,	1,
42,	70,	1,
43,	75,	1,
22,	80,	1,
7,      20,	1,
18,	12,	2,
4,      50,	1,
30,	10,	2,
]);