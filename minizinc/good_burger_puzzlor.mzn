% 
% Good Burger (PuzzlOR) in MiniZinc.
% 
% http://www.puzzlor.com/2014-08_GoodBurger.html
% """
% 
% Item        Sodium(mg)  Fat(g)  Calories  Item cost($)
% Beef Patty  50          17      220       $0.25
% Bun         330         9       260       $0.15
% Cheese      310         6       70        $0.10
% Onions      1           2       10        $0.09
% Pickles     260         0       5         $0.03
% Lettuce     3           0       4         $0.04
% Ketchup     160         0       20        $0.02
% Tomato      3           0       9         $0.04

% As the owner of a fast food restaurant with declining sales, your customers 
% are looking for something new and exciting on the menu. Your market research 
% indicates that they want a burger that is loaded with everything as long as it 
% meets certain health requirements. Money is no object to them.
% 
% The ingredient list in Table 1 shows what is available to include on the burger. 
% You must include at least one of each item and no more than five of each item. 
% You must use whole items (for example, no half servings of cheese). The final burger 
% must contain less than 3,000 mg of sodium, less than 150 grams of fat and less 
% than 3,000 calories.
% 
% To maintain certain taste quality standards you’ll need to keep the servings of 
% ketchup and lettuce the same. Also, you’ll need to keep the servings of pickles 
% and tomatoes the same.
%
% Question: 
%
% What is the most expensive burger you can make?
%
% Send your answer to puzzlor@gmail.com by Oct. 15 [2014]. The winner, chosen randomly 
% from correct answers, will receive a $25 Amazon Gift Card. Past questions can be 
% found at puzzlor.com.
% """

% Optimal solution (which happens to be unique)
%
% x: [5, 5, 1, 5, 1, 3, 3, 1]
% total_cost: 280 cent ($2.8)
%
% Beef Patty: 5
% Bun: 5
% Cheese: 1
% Onions: 5
% Pickles: 1
% Lettuce: 3
% Ketchup: 3
% Tomato: 1


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_items;
array[1..num_items, 1..4] of int: items;

% ingredients
int: sodium = 1;
int: fat = 2;
int: calories = 3;
int: cost = 4;

% items
int: beef_patty = 1;
int: bun = 2;
int: cheese = 3;
int: onions = 4;
int: pickles = 5;
int: lettuce = 6;
int: ketchup = 7;
int: tomato = 8;

% items (names) 
array[1..num_items] of string: items_names = ["Beef Patty","Bun","Cheese","Onions","Pickles","Lettuce","Ketchup","Tomato"];


% decision variables
array[1..num_items] of var 1..5: x;
var int: total_cost = sum([x[i]*items[i,4] | i in 1..num_items ]);

% solve satisfy;
solve maximize total_cost;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % 
  % """
  % You must use whole items (for example, no half servings of cheese). The final burger 
  % must contain less than 3,000 mg of sodium, less than 150 grams of fat and less 
  % than 3,000 calories.
  % """
  sum([x[i]*items[i,sodium] | i in 1..num_items ]) < 3000
  /\ sum([x[i]*items[i,fat] | i in 1..num_items ]) < 150
  /\ sum([x[i]*items[i,calories] | i in 1..num_items ]) < 3000

  % 
  % """
  % To maintain certain taste quality standards you’ll need to keep the servings of 
  % ketchup and lettuce the same. Also, you’ll need to keep the servings of pickles 
  % and tomatoes the same.
  % """
  /\ x[ketchup] = x[lettuce]
  /\ x[pickles] = x[tomato]
 
  % /\ total_cost = 280
 
;

output [
  "x: ", show(x), "\n",
  "total_cost: ", show(total_cost), " cent ($", show_float(2,2,int2float(total_cost) / 100.0), ")\n",
]
++
[
  show(items_names[i]) ++ ": " ++ show(x[i]) ++ "\n"
  | i in 1..num_items
]
;

%
%
%

% Item        Sodium(mg)  Fat(g)  Calories  Item cost($)
% Beef Patty  50          17      220       $0.25
% Bun         330         9       260       $0.15
% Cheese      310         6       70        $0.10
% Onions      1           2       10        $0.09
% Pickles     260         0       5         $0.03
% Lettuce     3           0       4         $0.04
% Ketchup     160         0       20        $0.02
% Tomato      3           0       9         $0.04

num_items = 8;

% cost in cent
items = array2d(1..num_items, 1..4,
[
% sodium  fat  colories  cost (cent)
 50,      17,    220,     25,
330,       9,    260,     15,
310,       6,     70,     10,
  1,       2,     10,      9,
260,       0,      5,      3,
  3,       0,      4,      4,
160,       0,     20,      2,
  3,       0,      9,      4
]);


