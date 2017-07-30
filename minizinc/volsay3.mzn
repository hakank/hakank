% 
% Volsay problem in MiniZinc.
% 
% From OPL model volsay.mod.
%
% Now with array, and components data in arrays/matrix
% 
%
% Compare with the Comet model:
% http://www.hakank.org/comet/volsay3.co

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: gas = 1;
int: chloride = 2;
set of int: Products = 1..2;

int: nitrogen = 1;
int: hydrogen = 2;
int: chlorine = 3;
set of int: Components = 1..3;


array[Products, Components] of float: Demand = array2d(Products, Components, 
        [ 
          1.0, 3.0, 0.0,
          1.0, 4.0, 1.0
        ]);

array[Products] of float: Profit =  [30.0, 40.0];
array[Components] of float: Stock = [50.0, 180.0, 40.0];

% decision variables
array[Products] of var 0.0..10000.0: Production;

var 0.0..100000.0: z = sum(p in Products) (Profit[p]*Production[p]);

solve maximize z;

constraint
   forall(c in Components) (
     sum(p in Products) (Demand[p,c]* Production[p]) <= Stock[c]
   )


;

output [
"Production: " ++ show(Production) ++ "\n"++
"z: " ++ show(z) ++ "\n"
];
