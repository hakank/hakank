% 
% Knapsack problem/0-1 in MiniZinc.
% 
% From Rosetta code:
% http://rosettacode.org/wiki/Knapsack_problem/0-1
% """
% A tourist wants to make a good trip at the weekend with his friends. They 
% will go to the mountains to see the wonders of nature, so he needs to 
% pack well for the trip. He has a good knapsack for carrying things, but 
% knows that he can carry a maximum of only 4kg in it and it will have 
% to last the whole day. He creates a list of what he wants to bring for the 
% trip but the total weight of all items is too much. He then decides to 
% add columns to his initial list detailing their weights and a numerical 
% value representing how important the item is for the trip.
%
% Here is the list:
% Table of potential knapsack items 
% item 	weight (dag) 	value
% map 	9 	150
% compass 	13 	35
% water 	153 	200
% sandwich 	50 	160
% glucose 	15 	60
% tin 	68 	45
% banana 	27 	60
% apple 	39 	40
% cheese 	23 	30
% beer 	52 	10
% suntan cream 	11 	70
% camera 	32 	30
% T-shirt 	24 	15
% trousers 	48 	10
% umbrella 	73 	40
% waterproof trousers 	42 	70
% waterproof overclothes 	43 	75
% note-case 	22 	80
% sunglasses 	7 	20
% towel 	18 	12
% socks 	4 	50
% book 	30 	10
% knapsack 	<=400 dag 	 ?
%
% The tourist can choose to take any combination of items from the list, but 
% only one of each item is available. He may not cut or diminish the items, 
% so he can only take whole units of any item.
% 
% Which items does the tourist carry in his knapsack so that their total weight 
% does not exceed 400 dag [4 kg], and their total value is maximised?
% 
% [dag = decagram = 10 grams] 
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: num_items = 22;
array[1..num_items, 1..2] of int: items;
array[1..num_items] of string: items_str;

% decision variables
array[1..num_items] of var 0..1: x;
var int: total_weight = sum(i in 1..num_items) ( x[i]*items[i,1] );
var int: total_value  = sum(i in 1..num_items) ( x[i]*items[i,2] );

solve :: int_search(
        x, 
        input_order, 
        indomain_max, 
        complete) 
    maximize total_value;
    % satisfy;


constraint
    total_weight <= 400 

    % /\ total_value = 1030 % testing all optimal solutions
;

output 
[
  "total_value: " ++ show(total_value) ++ "\n" ++ 
  "total_weight: " ++ show(total_weight) ++ "\n"
] ++
[
   if fix(x[i]) > 0 then 
     show(items_str[i]) ++ ": " ++ show(x[i]) ++ " value: " ++ show(items[i,2]) ++ "\n"
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
[
"map","compass","water","sandwich","glucose","tin","banana","apple","cheese",
"beer","suntancream","camera","T-shirt","trousers","umbrella","waterproof trousers",
"waterproof overclothes","note-case","sunglasses","towel","socks","book"];


%
% weight (dag) (each)  value (each)  pieces(s)
%
items = array2d(1..num_items, 1..2, [
9,       150,
13,      35,
153,     200,
50,      160,
15,      60,
68,      45,
27,      60,
39,      40,
23,      30,
52,      10,
11,      70,
32,      30,
24,      15,
48,      10,
73,      40,
42,      70,
43,      75,
22,      80,
7,       20,
18,      12,
4,       50,
30,      10
]);