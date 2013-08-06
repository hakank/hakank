% 
%  Fixed-charge problem in MiniZinc.
%  
%  
%  From the OPL documentation, and example model fixed.mod:
%  """
%  Fixed-charge problems are another classic application of integer programs 
%  (see Applications and Algorithms by W. Winston in the Bibliography). 
%  They resemble some of the production problems seen previously but differ 
%  in two respects: the production is an integer value (e.g., a factory 
%  must produce bikes or toys), and the factories need to rent (or acquire) 
%  some tools to produce some of the products. Consider the following 
%  problem. A company manufactures shirts, shorts, and pants. Each 
%  product requires a number of hours of labor and a certain amount of 
%  cloth, and the company has a limited capacity of both. In addition, 
%  all of these products can be manufactured only by renting an appropriate 
%  machine. The profit on the products (excluding the cost of renting 
%  the equipment) are also known. The company would like to maximize 
%  its profit.
%  """
%
%  Compare with the following model:
%  * Comet: http://www.hakank.org/comet/fixed_charge.co

%  
%  This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
%  See also my MiniZinc page: http://www.hakank.org/minizinc
%
%  include "globals.mzn"; 

int: shirtM = 1;
int: shortM = 2;
int: pantM = 3;
int: num_machines = 3;
set of int: Machines = {shirtM, shortM, pantM};

int: shirt = 1;
int: shorts = 2;
int: pants = 3;
int: num_products = 3;
set of 1..num_products: Products = {shirt, shorts, pants};

int: labor = 1;
int: cloth = 2;
int: num_resources = 2;
set of 1..num_resources: Resources = {labor, cloth};


array[1..num_resources] of int: Capacity= [ 150, 160 ];
int: MaxProduction = max(Capacity);
array[1..num_machines] of int: RentingCost = [ 200, 150, 100 ];

array[1..num_products] of int: ProductProfit = [6,4,7];

array[1..num_products] of set of int: ProductMachines = 
    [
       {shirtM},
       {shortM},
       {pantM}
    ];


array[1..num_products, 1..2] of int: use = array2d(1..num_products, 1..2,
    [
       3,4,
       2,3,
       6,4
    ]);


% decision variables
array[1..num_machines] of var 0..1: Rent;
array[1..num_products] of var 0..MaxProduction: Produce; 

var int: z =  
                sum( p in 1..num_products ) (ProductProfit[p] * Produce[p]) -
                sum( m in Machines ) (RentingCost[m] * Rent[m]);

solve maximize z;

constraint
  forall( r in Resources ) (
    sum( p in 1..num_products ) ( use[p,r] * Produce[p] )  <= Capacity[r]
  )
  /\
  forall( p in 1..num_products , mm in ProductMachines[p] ) (
    Produce[p] <= MaxProduction * Rent[mm]
  )
;


output
[
  "Rent: " ++ show(Rent) ++ "\n" ++
  "Produce: " ++ show(Produce) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];
