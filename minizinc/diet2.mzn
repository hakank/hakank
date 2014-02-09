% 
% Diet problem in MiniZinc.
% 
% From the GLPK model diet.mod
% """
%
% STIGLER'S NUTRITION MODEL
%
% This model determines a least cost diet which meets the daily
% allowances of nutrients for a moderately active man weighing 154 lbs.
%
% References:
%            Dantzig G B, "Linear Programming and Extensions."
%            Princeton University Press, Princeton, New Jersey, 1963,
%            Chapter 27-1.
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% nutrients 
set of int: N = 1..n;

% foods 
set of int: F = 1..num_foods;

% required daily allowances of nutrients 
array[N] of float: b;

% nutritive value of foods (per dollar spent) 
array[F, N] of 0.0..2000.0: a;

% dollars of food f to be purchased daily
array[F] of var 0.0..1.0: x;

%  total food bill (dollars) 
var float: cost = sum(f in F) (x[f]);

solve minimize cost;
% solve :: float_search(x++[a[i,j] |i in F, j in N]++b, 0.000001, input_order,indomain_split, complete) minimize cost;

constraint
  cost >= 0.0 /\
  
%  nutrient balance (units) 
  forall(n in N) (
     sum(f in F) (a[f,n] * x[f]) = b[n]
  )
;


%
% data
% 
int: n = 9;
int: num_foods = 20;

% Calorie       3 %  thousands 
% Protein      70 %  grams 
% Calcium     0.8 %  grams 
% Iron         12 %  milligrams 
% Vitamin-A     5 %  thousands IUs 
% Vitamin-B1  1.8 %  milligrams 
% Vitamin-B2  2.7 %  milligrams 
% Niacin       18 %  milligrams 
% Vitamin-C    75 %  milligrams   ;
b = [3.0, 70.0, 0.8, 12.0, 5.0, 1.8, 2.7, 18.0, 75.0];


a = array2d(F, N, [
%  Calorie  Protein  Calcium  Iron  Vitamin-A  Vitamin-B1 Vitamin-B2  Niacin  Vitamin-C 
% (1000)    (g)      (g)      (mg)  (1000IU)   (mg)       mg)         (mg)    (mg)
44.7,  1411.0,   2.0, 365.0,   0.0,  55.4,  33.3,   441.0,    0.0,
36.0,   897.0,   1.7,  99.0,  30.9,  17.4,   7.9,   106.0,    0.0,
 8.4,   422.0,  15.1,   9.0,  26.0,   3.0,  23.5,    11.0,   60.0,
20.6,    17.0,   0.6,   6.0,  55.8,   0.2,   0.0,     0.0,    0.0,
 7.4,   448.0,  16.4,  19.0,  28.1,   0.8,  10.3,     4.0,    0.0,
15.7,   661.0,   1.0,  48.0,   0.0,   9.6,   8.1,   471.0,    0.0,
41.7,   0.0,     0.0,   0.0,   0.2,   0.0,   0.5,     5.0,    0.0,
 2.2,   333.0,   0.2, 139.0, 169.2,   6.4,  50.8,   316.0,  525.0,
 4.4,   249.0,   0.3,  37.0,   0.0,  18.2,   3.6,    79.0,    0.0,
 5.8,   705.0,   6.8,  45.0,   3.5,   1.0,   4.9,   209.0,    0.0,
 2.4,   138.0,   3.7,  80.0,  69.0,   4.3,   5.8,    37.0,  862.0,
 2.6,   125.0,   4.0,  36.0,   7.2,   9.0,   4.5,    26.0,  5369.0,
 5.8,   166.0,   3.8,  59.0,  16.6,   4.7,   5.9,    21.0,  1184.0,
14.3,   336.0,   1.8, 118.0,   6.7,  29.4,   7.1,   198.0,  2522.0,
 1.1,   106.0,   0.0, 138.0, 918.4,   5.7,  13.8,    33.0,  2755.0,
 9.6,   138.0,   2.7,  54.0, 290.7,   8.4,   5.4,    83.0,  1912.0,
 8.5,    87.0,   1.7, 173.0,  86.8,   1.2,   4.3,    55.0,    57.0,
12.8,    99.0,   2.5, 154.0,  85.7,   3.9,   4.3,    65.0,   257.0,
17.4,  1055.0,   3.7, 459.0,   5.1,  26.9,  38.2,    93.0,     0.0,
26.9,  1691.0,  11.4, 792.0,   0.0,  38.4,  24.6,   217.0,     0.0]);



output [
 "Dollars of food f to be purchased daily:\n",
 "Wheat      : $", show(x[1]), "\n",
 "Cornmeal   : $", show(x[2]), "\n",
 "Cannedmilk : $", show(x[3]), "\n",
 "Margarine  : $", show(x[4]), "\n",
 "Cheese     : $", show(x[5]), "\n",
 "Peanut-B   : $", show(x[6]), "\n",
 "Lard       : $", show(x[7]), "\n",
 "Liver      : $", show(x[8]), "\n",
 "Porkroast  : $", show(x[9]), "\n",
 "Salmon     : $", show(x[10]), "\n",
 "Greenbeans : $", show(x[11]), "\n",
 "Cabbage    : $", show(x[12]), "\n",
 "Onions     : $", show(x[13]), "\n",
 "Potatoes   : $", show(x[14]), "\n",
 "Spinach    : $", show(x[15]), "\n",
 "Sweet-Pot  : $", show(x[16]), "\n",
 "Peaches    : $", show(x[17]), "\n",
 "Prunes     : $", show(x[18]), "\n",
 "Limabeans  : $", show(x[19]), "\n",
 "Navybeans  : $", show(x[20]), "\n",
 "Total cost : $", show(cost), "\n",
 "a: ", show(a), "\n",
 "b: ", show(b), "\n"
];