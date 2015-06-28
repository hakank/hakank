% 
% Make a Good Burger problem in MiniZinc.
% 
% From
% https://dmcommunity.wordpress.com/challenge/make-a-good-burger/
% """
% As the owner of a fast food restaurant with declining sales, you know that your 
% customers are looking for something new and exciting on the menu.  Your market research 
% indicates that they want a burger that is loaded with everything as long as it 
% meets certain health requirements.  Money is no object to them. The ingredient list in 
% the table below shows what is available to include on the burger:

% Item       Sodium(mg)   Fat(g)   Calories   Item costs ($)
% BeefPatty   50           17       220         $0.25
% Bun        330            9       260         $0.15
% Cheese     310            6        70         $0.10
% Onions       1            2        10         $0.09
% Pickles    260            0         5         $0.03
% Lettuce      3            0         4         $0.04
% Ketchup    160            0        20         $0.02
% Tomato       3            0         9         $0.04

% You must include at least one of each item and no more than five of 
% each item.  You must use whole items (for example, no half servings 
% of cheese).  The final burger must contain 
%    less than 3000 mg of sodium, 
%    less than 150 grams of fat, 
%    and less than 3000 calories. 
%
% To maintain certain taste quality standards you’ll need to keep the 
% servings of ketchup and lettuce the same.  Also, you’ll need to 
% keep the servings of pickles and tomatoes the same.
% 
% Question:  Offer several recipes for a good burger. What is the most 
% and the less expensive burger you can make?
% """

% Min:
% total: 72 cent
% x: [1, 1, 1, 1, 1, 1, 1, 1]

% Max:
% total: 280 cent
% x: [5, 5, 1, 5, 1, 3, 3, 1]


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: num_items;
array[1..num_items, 1..4] of int: items;
array[1..num_items] of string: names;

int: num_limits;
array[1..num_limits] of int: limits;

% decision variables
array[1..num_items] of var 1..5: x;
var int: total = sum([x[i]*items[i,4] | i in 1..num_items]);

% solve minimize total;
solve maximize total;

constraint
   % sodium < 3000mg, fat < 150g, calories < 3000
   forall(j in 1..num_limits) (
     sum([x[i]*items[i,j] | i in 1..num_items]) < limits[j]
   )

   % keep the serving of ketchup and lettuce the same.  
   /\ x[Ketchup] = x[Lettuce]

   % keep the servings of pickles and tomatoes the same.
   /\ x[Pickles] = x[Tomato]
;


output [
  "total   : ", show(total), " cent\n",
  "sodium  : ", show(sum([x[i]*items[i,1] | i in 1..num_items])), " mg\n",
  "fat     : ", show(sum([x[i]*items[i,2] | i in 1..num_items])), " g\n",
  "calories: ", show(sum([x[i]*items[i,3] | i in 1..num_items])), "\n",
  "x       : ", show(x), "\n",
]
++
[
  show(names[i]) ++ ": " ++ show(x[i]) ++ " (" ++ show_int(3,x[i]*items[i,4]) ++ " cent)\n"
  | i in 1..num_items
]
;



% Item       Sodium(mg)   Fat(g)   Calories   Item costs ($)
% 1 BeefPatty   50           17       220         $0.25
% 2 Bun        330            9       260         $0.15
% 3 Cheese     310            6        70         $0.10
% 4 Onions       1            2        10         $0.09
% 5 Pickles    260            0         5         $0.03
% 6 Lettuce      3            0         4         $0.04
% 7 Ketchup    160            0        20         $0.02
% 8 Tomato       3            0         9         $0.04

num_limits = 3;
%        sodium fat calories
limits = [3000,150,3000];

num_items = 8;
items = array2d(1..num_items, 1..4,
[
% sodium       fat       calories   cost (cent)
 50,           17,       220,       25,
330,            9,       260,       15,
310,           6,         70,       10,
  1,            2,        10,        9,
260,            0,         5,        3,
  3,            0,         4,        4,
160,            0,        20,        2,
  3,            0,         9,        4,
]);

int: BeefPatty = 1;
int: Bun       = 2;
int: Cheese    = 3;
int: Onions    = 4;
int: Pickles   = 5;
int: Lettuce   = 6;
int: Ketchup   = 7;
int: Tomato    = 8;

names = ["Beef Patty", 
         "Bun       ", 
         "Cheese    ", 
         "Onion     ",
         "Pickles   ",
         "Lettuce   ", 
         "Ketchup   ", 
         "Tomato    "];
