% 
% Original Stigler's 1939 diet problem in MiniZinc.
% 
% From GLPK:s example stigler.mod
% """
% STIGLER, original Stigler's 1939 diet problem
%
% The Stigler Diet is an optimization problem named for George Stigler,
% a 1982 Nobel Laureate in economics, who posed the following problem:
% For a moderately active man weighing 154 pounds, how much of each of
% 77 foods should be eaten on a daily basis so that the man's intake of
% nine nutrients will be at least equal to the recommended dietary
% allowances (RDSs) suggested by the National Research Council in 1943,
% with the cost of the diet being minimal?
%
% The nutrient RDAs required to be met in Stigler's experiment were
% calories, protein, calcium, iron, vitamin A, thiamine, riboflavin,
% niacin, and ascorbic acid. The result was an annual budget allocated
% to foods such as evaporated milk, cabbage, dried navy beans, and beef
% liver at a cost of approximately $0.11 a day in 1939 U.S. dollars.
%
% While the name "Stigler Diet" was applied after the experiment by
% outsiders, according to Stigler, "No one recommends these diets for
% anyone, let alone everyone." The Stigler diet has been much ridiculed
% for its lack of variety and palatability, however his methodology has
% received praise and is considered to be some of the earliest work in
% linear programming.
%
% The Stigler diet question is a linear programming problem. Lacking
% any sophisticated method of solving such a problem, Stigler was
% forced to utilize heuristic methods in order to find a solution. The
% diet question originally asked in which quantities a 154 pound male
% would have to consume 77 different foods in order to fulfill the
% recommended intake of 9 different nutrients while keeping expense at
% a minimum. Through "trial and error, mathematical insight and
% agility," Stigler was able to eliminate 62 of the foods from the
% original 77 (these foods were removed based because they lacked
% nutrients in comparison to the remaining 15). From the reduced list,
% Stigler calculated the required amounts of each of the remaining 15
% foods to arrive at a cost-minimizing solution to his question.
% According to Stigler's calculations, the annual cost of his solution
% was $39.93 in 1939 dollars. When corrected for inflation using the
% consumer price index, the cost of the diet in 2005 dollars is
% $561.43. The specific combination of foods and quantities is as
% follows:
%
% Stigler's 1939 Diet
%
% Food             Annual Quantities Annual Cost
% ---------------- ----------------- -----------
% Wheat Flour           370 lb.         $13.33
% Evaporated Milk        57 cans          3.84
% Cabbage               111 lb.           4.11
% Spinach                23 lb.           1.85
% Dried Navy Beans      285 lb.          16.80
% ----------------------------------------------
% Total Annual Cost                     $39.93
%
% The 9 nutrients that Stigler's diet took into consideration and their
% respective recommended daily amounts were:
%
% Table of nutrients considered in Stigler's diet
%
% Nutrient                  Daily Recommended Intake
% ------------------------- ------------------------
% Calories                       3,000 Calories
% Protein                           70 grams
% Calcium                           .8 grams
% Iron                              12 milligrams
% Vitamin A                      5,000 IU
% Thiamine (Vitamin B1)            1.8 milligrams
% Riboflavin (Vitamin B2)          2.7 milligrams
% Niacin                            18 milligrams
% Ascorbic Acid (Vitamin C)         75 milligrams
%
% Seven years after Stigler made his initial estimates, the development
% of George Dantzig's Simplex algorithm made it possible to solve the
% problem without relying on heuristic methods. The exact value was
% determined to be $39.69 (using the original 1939 data). Dantzig's
% algorithm describes a method of traversing the vertices of a polytope
% of N+1 dimensions in order to find the optimal solution to a specific
% situation.
%
% (From Wikipedia, the free encyclopedia.)
%
% Translated from GAMS by Andrew Makhorin <mao@mai2.rcnet.ru>.
%
% For the original GAMS model stigler1939.gms see [3].
%
% References:
%
% 1. George J. Stigler, "The Cost of Subsistence," J. Farm Econ. 27,
%    1945, pp. 303-14.
%
% 2. National Research Council, "Recommended Daily Allowances," Reprint
%    and Circular Series No. 115, January, 1943.
%
% 3. Erwin Kalvelagen, "Model building with GAMS," Chapter 2, "Building
%    linear programming models," pp. 128-34.
% """
 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


% Solution according GLPK. (ECLiPSe/eplex and MiniZinc/mip gives the same result.)
% """
%
%         Commodity            Unit     Quantity     Cost
% ------------------------- ---------- ---------- ----------
% Wheat Flour (Enriched)        10 lb.      29.95   $  10.78
% Liver (Beef)                   1 lb.       2.58   $   0.69
% Cabbage                        1 lb.     110.70   $   4.10
% Spinach                        1 lb.      22.58   $   1.83
% Navy Beans, Dried              1 lb.     377.81   $  22.29
%                                          -----------------
%                                          Total:   $  39.69
% """



% commodities
int: num_commodities = 77; 
set of int: C = 1..num_commodities;

% days in a year 
float: days = 365.25;

% nutrients 
int: num_nutrients = 9;
set of int: N = 1..num_nutrients;

% nutritive values per dollar of expenditure 
% [price and weight are the two first columns in data array]
array[C, 1..num_nutrients+2] of float: data; %  in C, {"price", "weight"} union N};

% recommended daily allowance for a moderately active man 
array[N] of float: allowance;


% dollars of food to be purchased daily 
array[C] of var 0.0..1.0: x;
array[C] of var 0.0..100.0: x_cost;
array[C] of var 0.0..1000.0: quant;

% total food bill 
var float: total_cost = days * sum(c in C) (x[c]);
var float: cost = sum(c in C) (x[c]); % cost per day

solve minimize cost;
% solve :: float_search(x_cost ++ quant ++ x, 0.01, input_order, indomain_split,complete) minimize cost;

constraint
  cost >= 0.0 /\

  % nutrient balance 
  forall(n in 3..num_nutrients+2) (
      sum(c in C) (data[c,n] * x[c]) >= allowance[n-2]
  )
  /\
  forall(c in C) (
    x[c] >= 0.0
    /\
    x_cost[c] >= 0.0
    /\
    quant[c] >= 0.0
    /\
    x_cost[c] = days * x[c]
    /\
    quant[c] = 100.0*days*x[c] / data[c,1]
  )
;

output [
  "cost: ", show(cost), "\n",
  "x: ", show(x), "\n",
  "x_cost: ", show(x_cost), "\n",
  "quant: ", show(quant), "\n",
  "total_cost: ", show(total_cost), "\n",

];


%
% data
%


% param : C :    commodity                   unit :=
% flour          "Wheat Flour (Enriched)"    "10 lb."
% macaroni       "Macaroni"                  "1 lb."
% cereal         "Wheat Cereal (Enriched)"   "28 oz."
% cornflakes     "Corn Flakes"               "8 oz."
% cornmeal       "Corn Meal"                 "1 lb."
% grits          "Hominy Grits"              "24 oz."
% rice           "Rice"                      "1 lb."
% oats           "Rolled Oats"               "1 lb."
% whitebread     "White Bread (Enriched)"    "1 lb."
% wheatbread     "Whole Wheat Bread"         "1 lb."
% ryebread       "Rye Bread"                 "1 lb."
% poundcake      "Pound Cake"                "1 lb."
% crackers       "Soda Crackers"             "1 lb."
% milk           "Milk"                      "1 qt."
% evapmild       "Evaporated Milk (can)"     "14.5 oz."
% butter         "Butter"                    "1 lb."
% margarine      "Oleomargarine"             "1 lb."
% eggs           "Eggs"                      "1 doz."
% cheese         "Cheese (Cheddar)"          "1 lb."
% cream          "Cream"                     "1/2 pt."
% peanutbutter   "Peanut Butter"             "1 lb."
% mayonnaise     "Mayonnaise"                "1/2 pt."
% crisco         "Crisco"                    "1 lb."
% lard           "Lard"                      "1 lb."
% sirloinsteak   "Sirloin Steak"             "1 lb."
% roundsteak     "Round Steak"               "1 lb."
% ribroast       "Rib Roast"                 "1 lb."
% chuckroast     "Chuck Roast"               "1 lb."
% plate          "Plate"                     "1 lb."
% liver          "Liver (Beef)"              "1 lb."
% lambleg        "Leg of Lamb"               "1 lb."
% lambchops      "Lamb Chops (Rib)"          "1 lb."
% porkchops      "Pork Chops"                "1 lb."
% porkroast      "Pork Loin Roast"           "1 lb."
% bacon          "Bacon"                     "1 lb."
% ham            "Ham - smoked"              "1 lb."
% saltpork       "Salt Pork"                 "1 lb."
% chicken        "Roasting Chicken"          "1 lb."
% veal           "Veal Cutlets"              "1 lb."
% salmon         "Salmon, Pink (can)"        "16 oz."
% apples         "Apples"                    "1 lb."
% bananas        "Bananas"                   "1 lb."
% lemons         "Lemons"                    "1 doz."
% oranges        "Oranges"                   "1 doz."
% greenbeans     "Green Beans"               "1 lb."
% cabbage        "Cabbage"                   "1 lb."
% carrots        "Carrots"                   "1 bunch"
% celery         "Celery"                    "1 stalk"
% lettuce        "Lettuce"                   "1 head"
% onions         "Onions"                    "1 lb."
% potatoes       "Potatoes"                  "15 lb."
% spinach        "Spinach"                   "1 lb."
% sweetpotato    "Sweet Potatoes"            "1 lb."
% peaches        "Peaches (can)"             "No. 2 1/2"
% pears          "Pears (can)"               "No. 2 1/2"
% pineapple      "Pineapple (can)"           "No. 2 1/2"
% asparagus      "Asparagus (can)"           "No. 2"
% cannedgrbn     "Grean Beans (can)"         "No. 2"
% porkbeans      "Pork and Beans (can)"      "16 oz."
% corn           "Corn (can)"                "No. 2"
% peas           "Peas (can)"                "No. 2"
% tomatoes       "Tomatoes (can)"            "No. 2"
% tomatosoup     "Tomato Soup (can)"         "10 1/2 oz."
% driedpeach     "Peaches, Dried"            "1 lb."
% prunes         "Prunes, Dried"             "1 lb."
% raisins        "Raisins, Dried"            "15 oz."
% driedpeas      "Peas, Dried"               "1 lb."
% limabeans      "Lima Beans, Dried"         "1 lb."
% navybeans      "Navy Beans, Dried"         "1 lb."
% coffee         "Coffee"                    "1 lb."
% tea            "Tea"                       "1/4 lb."
% cocoa          "Cocoa"                     "8 oz."
% chocolate      "Chocolate"                 "8 oz."
% sugar          "Sugar"                     "10 lb."
% cornsirup      "Corn Sirup"                "24 oz."
% molasses       "Molasses"                  "18 oz."
% stawberry      "Strawberry Preserve"       "1 lb."
% ;

% set N :=
% calories       % Calories, unit = 1000 
% protein        % Protein, unit = grams 
% calcium        % Calcium, unit = grams 
% iron           % Iron, unit = milligrams 
% vitaminA       % Vitamin A, unit = 1000 International Units 
% thiamine       % Thiamine, Vit. B1, unit = milligrams 
% riboflavin     % Riboflavin, Vit. B2, unit = milligrams 
% niacin         % Niacin (Nicotinic Acid), unit = milligrams 
% ascorbicAcid   % Ascorbic Acid, Vit. C, unit = milligrams 
% ;

% price and weight is the two first columns
data = array2d(C, 1..num_nutrients+2, [
 36.0,  12600.0,  44.7,   1411.0,   2.0,    365.0,   0.0,  55.4,     33.3,     441.0,     0.0,
 14.1,   3217.0,  11.6,    418.0,   0.7,     54.0,   0.0,   3.2,    1.9,    68.0,     0.0,
 24.2,   3280.0,  11.8,    377.0,  14.4,    175.0,   0.0,  14.4,    8.8,     114.0,     0.0,
  7.1,   3194.0,  11.4,    252.0,   0.1,     56.0,   0.0,  13.5,    2.3,    68.0,     0.0,
  4.6,   9861.0,  36.0,    897.0,   1.7,     99.0,  30.9,  17.4,    7.9,     106.0,     0.0,
  8.5,   8005.0,  28.6,    680.0,   0.8,     80.0,   0.0,  10.6,    1.6,     110.0,     0.0,
  7.5,   6048.0,  21.2,    460.0,   0.6,     41.0,   0.0,   2.0,    4.8,    60.0,     0.0,
  7.1,   6389.0,  25.3,    907.0,   5.1,    341.0,   0.0,  37.1,    8.9,    64.0,     0.0,
  7.9,   5742.0,  15.6,    488.0,   2.5,    115.0,   0.0,  13.8,    8.5,     126.0,     0.0,
  9.1,   4985.0,  12.2,    484.0,   2.7,    125.0,   0.0,  13.9,    6.4,     160.0,     0.0,
  9.2,   4930.0,  12.4,    439.0,   1.1,     82.0,   0.0,   9.9,    3.0,    66.0,     0.0,
 24.8,   1829.0,   8.0,    130.0,   0.4,     31.0,  18.9,   2.8,    3.0,    17.0,     0.0,
 15.1,   3004.0,  12.5,    288.0,   0.5,     50.0,   0.0,   0.0,    0.0,     0.0,     0.0,
 11.0,   8867.0,   6.1,    310.0,  10.5,     18.0,  16.8,   4.0,     16.0,     7.0,     177.0,
  6.7,   6035.0,   8.4,    422.0,  15.1,    9.0,  26.0,   3.0,     23.5,    11.0,    60.0,
 20.8,   1473.0,  10.8,    9.0,   0.2,    3.0,  44.2,   0.0,    0.2,     2.0,     0.0,
 16.1,   2817.0,  20.6,     17.0,   0.6,    6.0,  55.8,   0.2,    0.0,     0.0,     0.0,
 32.6,   1857.0,   2.9,    238.0,   1.0,     52.0,  18.6,   2.8,    6.5,     1.0,     0.0,
 24.2,   1874.0,   7.4,    448.0,  16.4,     19.0,  28.1,   0.8,     10.3,     4.0,     0.0,
 14.1,   1689.0,   3.5,     49.0,   1.7,    3.0,  16.9,   0.6,    2.5,     0.0,    17.0,
 17.9,   2534.0,  15.7,    661.0,   1.0,     48.0,   0.0,   9.6,    8.1,     471.0,     0.0,
 16.7,   1198.0,   8.6,     18.0,   0.2,    8.0,   2.7,   0.4,    0.5,     0.0,     0.0,
 20.3,   2234.0,  20.1,    0.0,   0.0,    0.0,   0.0,   0.0,    0.0,     0.0,     0.0,
  9.8,   4628.0,  41.7,    0.0,   0.0,    0.0,   0.2,   0.0,    0.5,     5.0,     0.0,
 39.6,   1145.0,   2.9,    166.0,   0.1,     34.0,   0.2,   2.1,    2.9,    69.0,     0.0,
 36.4,   1246.0,   2.2,    214.0,   0.1,     32.0,   0.4,   2.5,    2.4,    87.0,     0.0,
 29.2,   1553.0,   3.4,    213.0,   0.1,     33.0,   0.0,   0.0,    2.0,     0.0,     0.0,
 22.6,   2007.0,   3.6,    309.0,   0.2,     46.0,   0.4,   1.0,    4.0,     120.0,     0.0,
 14.6,   3107.0,   8.5,    404.0,   0.2,     62.0,   0.0,   0.9,    0.0,     0.0,     0.0,
 26.8,   1692.0,   2.2,    333.0,   0.2,    139.0, 169.2,   6.4,     50.8,     316.0,     525.0,
 27.6,   1643.0,   3.1,    245.0,   0.1,     20.0,   0.0,   2.8,    3.0,    86.0,     0.0,
 36.6,   1239.0,   3.3,    140.0,   0.1,     15.0,   0.0,   1.7,    2.7,    54.0,     0.0,
 30.7,   1477.0,   3.5,    196.0,   0.2,     80.0,   0.0,  17.4,    2.7,    60.0,     0.0,
 24.2,   1874.0,   4.4,    249.0,   0.3,     37.0,   0.0,  18.2,    3.6,    79.0,     0.0,
 25.6,   1772.0,  10.4,    152.0,   0.2,     23.0,   0.0,   1.8,    1.8,    71.0,     0.0,
 27.4,   1655.0,   6.7,    212.0,   0.2,     31.0,   0.0,   9.9,    3.3,    50.0,     0.0,
 16.0,   2835.0,  18.8,    164.0,   0.1,     26.0,   0.0,   1.4,    1.8,     0.0,     0.0,
 30.3,   1497.0,   1.8,    184.0,   0.1,     30.0,   0.1,   0.9,    1.8,    68.0,    46.0,
 42.3,   1072.0,   1.7,    156.0,   0.1,     24.0,   0.0,   1.4,    2.4,    57.0,     0.0,
 13.0,   3489.0,   5.8,    705.0,   6.8,     45.0,   3.5,   1.0,    4.9,     209.0,     0.0,
  4.4,   9072.0,   5.8,     27.0,   0.5,     36.0,   7.3,   3.6,    2.7,     5.0,     544.0,
  6.1,   4982.0,   4.9,     60.0,   0.4,     30.0,  17.4,   2.5,    3.5,    28.0,     498.0,
 26.0,   2380.0,   1.0,     21.0,   0.5,     14.0,   0.0,   0.5,    0.0,     4.0,     952.0,
 30.9,   4439.0,   2.2,     40.0,   1.1,     18.0,  11.1,   3.6,    1.3,    10.0,    1993.0,
  7.1,   5750.0,   2.4,    138.0,   3.7,     80.0,  69.0,   4.3,    5.8,    37.0,     862.0,
  3.7,   8949.0,   2.6,    125.0,   4.0,     36.0,   7.2,   9.0,    4.5,    26.0,    5369.0,
  4.7,   6080.0,   2.7,     73.0,   2.8,     43.0, 188.5,   6.1,    4.3,    89.0,     608.0,
  7.3,   3915.0,   0.9,     51.0,   3.0,     23.0,   0.9,   1.4,    1.4,     9.0,     313.0,
  8.2,   2247.0,   0.4,     27.0,   1.1,     22.0, 112.4,   1.8,    3.4,    11.0,     449.0,
  3.6,  11844.0,   5.8,    166.0,   3.8,     59.0,  16.6,   4.7,    5.9,    21.0,    1184.0,
 34.0,  16810.0,  14.3,    336.0,   1.8,    118.0,   6.7,  29.4,    7.1,     198.0,    2522.0,
  8.1,   4592.0,   1.1,    106.0,   0.0,    138.0, 918.4,   5.7,     13.8,    33.0,    2755.0,
  5.1,   7649.0,   9.6,    138.0,   2.7,     54.0, 290.7,   8.4,    5.4,    83.0,    1912.0,
 16.8,   4894.0,   3.7,     20.0,   0.4,     10.0,  21.5,   0.5,    1.0,    31.0,     196.0,
 20.4,   4030.0,   3.0,    8.0,   0.3,    8.0,   0.8,   0.8,    0.8,     5.0,    81.0,
 21.3,   3993.0,   2.4,     16.0,   0.4,    8.0,   2.0,   2.8,    0.8,     7.0,     399.0,
 27.7,   1945.0,   0.4,     33.0,   0.3,     12.0,  16.3,   1.4,    2.1,    17.0,     272.0,
 10.0,   5386.0,   1.0,     54.0,   2.0,     65.0,  53.9,   1.6,    4.3,    32.0,     431.0,
  7.1,   6389.0,   7.5,    364.0,   4.0,    134.0,   3.5,   8.3,    7.7,    56.0,     0.0,
 10.4,   5452.0,   5.2,    136.0,   0.2,     16.0,  12.0,   1.6,    2.7,    42.0,     218.0,
 13.8,   4109.0,   2.3,    136.0,   0.6,     45.0,  34.9,   4.9,    2.5,    37.0,     370.0,
  8.6,   6263.0,   1.3,     63.0,   0.7,     38.0,  53.2,   3.4,    2.5,    36.0,    1253.0,
  7.6,   3917.0,   1.6,     71.0,   0.6,     43.0,  57.9,   3.5,    2.4,    67.0,     862.0,
 15.7,   2889.0,   8.5,     87.0,   1.7,    173.0,  86.8,   1.2,    4.3,    55.0,    57.0,
  9.0,   4284.0,  12.8,     99.0,   2.5,    154.0,  85.7,   3.9,    4.3,    65.0,     257.0,
  9.4,   4524.0,  13.5,    104.0,   2.5,    136.0,   4.5,   6.3,    1.4,    24.0,     136.0,
  7.9,   5742.0,  20.0,   1367.0,   4.2,    345.0,   2.9,  28.7,     18.4,     162.0,     0.0,
  8.9,   5097.0,  17.4,   1055.0,   3.7,    459.0,   5.1,  26.9,     38.2,    93.0,     0.0,
  5.9,   7688.0,  26.9,   1691.0,  11.4,    792.0,   0.0,  38.4,     24.6,     217.0,     0.0,
 22.4,   2025.0,   0.0,    0.0,   0.0,    0.0,   0.0,   4.0,    5.1,    50.0,     0.0,
 17.4,    652.0,   0.0,    0.0,   0.0,    0.0,   0.0,   0.0,    2.3,    42.0,     0.0,
  8.6,   2637.0,   8.7,    237.0,   3.0,     72.0,   0.0,   2.0,     11.9,    40.0,     0.0,
 16.2,   1400.0,   8.0,     77.0,   1.3,     39.0,   0.0,   0.9,    3.4,    14.0,     0.0,
 51.7,   8773.0,  34.9,    0.0,   0.0,    0.0,   0.0,   0.0,    0.0,     0.0,     0.0,
 13.7,   4996.0,  14.7,    0.0,   0.5,     74.0,   0.0,   0.0,    0.0,     5.0,     0.0,
 13.6,   3752.0,   9.0,    0.0,  10.3,    244.0,   0.0,   1.9,    7.5,     146.0,     0.0,
 20.5,   2213.0,   6.4,     11.0,   0.4,    7.0,   0.2,   0.2,    0.4,     3.0,     0.0]);


allowance = [3.0, 70.0, 0.8, 12.0,  5.0,  1.8, 2.7, 18.0, 75.0];

