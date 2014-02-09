% 
% Transportation problem in MiniZinc.
% 
% This is in princple the same model as in 
%  Pascal Van Hentenryck "The OPL Optimization Programming Language",
%  page 116.

%  Data from OPL example transp1.dat

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

% cities:
% GARY, CLEV, PITT, FRA,  DET,  LAN,  WIN, STL,  FRE,  LAF 
int: num_cities = 10;
set of int: Cities = 1..num_cities;

% products: bands, coils, plate
int: num_products = 3;
set of int: Products = 1..num_products;

float: Capacity = 625.0;

% I'm too lazy to add ".0" to all these entries, so
% they are defined as ints and then converted to float
% in the constraint section.
array[1..num_products,1..num_cities] of int: Supply = 
  array2d(1..num_products, 1..num_cities,
  [
   400, 700,800,0,0,0,0,0,0,0,
   800, 1600, 1800,0,0,0,0,0,0,0,
   200, 300, 300,0,0,0,0,0,0,0
   ]);

array[1..num_products,1..num_cities] of int: Demand = 
  array2d(1..num_products,1..num_cities,
[
 0, 0, 0,300,300,100,75,650,225,250,  % bands
 0, 0, 0,500,750,400,250,950,850,500, % coils
 0, 0, 0,100,100,0,50,200,100,250     % plate
 ]);

array[1..num_products, 1..num_cities,1..num_cities] of int: Cost = 
  array3d(1..num_products, 1..num_cities,1..num_cities,
   [
   % bands
       0,    0,    0,    30,  10,    8,   10,   11,   71,    6,
       0,    0,    0,    22,   7,   10,    7,   21,   82,   13,
       0,    0,    0,    19,  11,   12,   10,   25,   83,   15,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,     
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0    
   ,
    % coils
       0,    0,    0,   39,   14,   11,   14,   16,   82,    8,
       0,    0,    0,   27,    9,   12,    9,   26,   95,   17,
       0,    0,    0,   24,   14,   17,   13,   28,   99,   20,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0    
   ,
   % plate
       0,    0,    0,   41,   15,   12,   16,   17,   86,    8,
       0,    0,    0,   29,    9,   13,    9,   28,   99,   18,
       0,    0,    0,   26,   14,   17,   13,   31,  104,   20,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0    
     ]);

% decision variables
array[1..num_products,1..num_cities,1..num_cities] of var 0.0..1000000.0: Trans;
var 0.0..1000000.0: z;

% solve minimize z;
solve :: float_search([Trans[i,j,k] |i in 1..num_products,j,k in 1..num_cities],0.001,input_order,indomain_split,complete) minimize z;

constraint
  z = sum( p in Products , o in Cities, d in Cities ) (
         int2float(Cost[p,o,d]) * Trans[p,o,d]
  )
  /\
  forall( p in Products , o in Cities ) (
      sum( d in Cities ) (Trans[p,o,d]) = int2float(Supply[p,o])
  )
  /\
  forall( p in Products , d in Cities ) (
    sum( o in Cities ) (Trans[p,o,d]) == int2float(Demand[p,d])
  )
  /\
  forall( o in Cities, d in Cities ) (
     sum( p in Products ) (Trans[p,o,d]) <= Capacity
  )
;


output [
"Trans: " ++ show(Trans) ++ "\n"++
"z: " ++ show(z) ++ "\n"
];
