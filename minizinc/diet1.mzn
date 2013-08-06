%
% Standard Operations Research example in Minizinc
%
%
% Minimize the cost for the products:
% Type of                        Calories   Chocolate    Sugar    Fat
% Food                                      (ounces)     (ounces) (ounces)
% Chocolate Cake (1 slice)       400           3            2      2
% Chocolate ice cream (1 scoop)  200           2            2      4
% Cola (1 bottle)                150           0            4      1
% Pineapple cheesecake (1 piece) 500           0            4      5
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: n; 
int: p = 4;
array[1..n] of var int: x;
array[1..n] of var int: price;

array[1..p] of int: limits;
array[1..n] of int: calories;
array[1..n] of int: chocolate;
array[1..n] of int: sugar;
array[1..n] of int: fat;

var int: cost = sum(i in 1..n) (price[i]*x[i]); 

solve minimize cost ;
% solve satisfy;

constraint
          forall(i in index_set(x)) (x[i] >= 0)
       /\ sum(i in 1..n) (x[i]*calories[i])  >= limits[1]
       /\ sum(i in 1..n) (x[i]*chocolate[i]) >= limits[2]
       /\ sum(i in 1..n) (x[i]*sugar[i])     >= limits[3]
       /\ sum(i in 1..n) (x[i]*fat[i])       >= limits[4]
%        /\ cost <= 90 % for solve satisfy
;


% data
n = 4;
price  = [ 50, 20, 30, 80]; % in cents
limits = [500,  6, 10,  8]; % requirements for each nutrition type

% nutritions for each product
calories  = [400, 200, 150, 500];
chocolate = [3,2,0,0];
sugar     = [2,2,4,4];
fat       = [2,4,1,5];


output 
[
  "cost: " ++ show(cost) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
];