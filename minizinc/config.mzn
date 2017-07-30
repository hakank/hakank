% 
% Config problem in MiniZinc.
% 
% From OPL's example config.mod (and config.dat)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: nbModel = 3;
int: nbCard = 4;
int: nbRack = 5;

set of int: Models = 0..nbModel-1;
set of int: Cards  = 0..nbCard-1;
set of int: Racks  = 0..nbRack-1;

% modelType:
%   power
%   connectors
%   price
int: modelTypes = 3;
array[Models, 1..modelTypes] of int: model = 
     array2d(Models, 1..modelTypes,
           [  0,  0,   0,
            150,  8, 150,
            200, 16, 200
           ]);

% cardType:
%   power
%   quantity
int: cardTypes = 2;
array[Cards, 1..cardTypes] of int: car = array2d(Cards, 1..cardTypes,
         [  20, 10,
            40,  4 ,
            50,  2, 
            75,  1]);


int: maxPrice = max(r in Models) (model[r,3]);
int: maxCost = nbCard * maxPrice;
array[Models] of int: powerData = array1d(Models, [model[i,1] | i in Models]);
array[Models] of int: connData  = array1d(Models, [model[i,2] | i in Models]);
array[Models] of int: priceData = array1d(Models, [model[i,3] | i in Models]);

% decision variables
array[Racks] of var Models: rack;
array[Racks, Cards] of var 0..nbCard: counters;
var 0..maxCost: cost;


% solve minimize cost;
% solve satisfy;
solve :: int_search(
        rack ++ [counters[r,c] | r in Racks, c in Cards], 
        first_fail, 
        indomain_max, 
        complete) 
    minimize cost;

constraint

   forall(r in Racks) (
      sum(c in Cards) (counters[r,c] * car[c, 1]) <= powerData[rack[r]]
   )
   /\
   forall(r in Racks) (
      sum(c in Cards) (counters[r,c]) <= connData[rack[r]] 
   )
   /\
   forall(c in Cards) (
      sum(r in Racks) ( counters[r,c])  == car[c,2]
   )
   /\
   cost == sum(r in Racks) (priceData[rack[r]])
;   

% for solve satisfy
% constraint cost = 550;

output 
[
  "cost: " ++ show(cost) ++ "\n" ++
  "rack: " ++ show(rack) ++ "\n"
  % ++ "counters: " 
]
% ++
% [ if c = 0 then "\n" else " " endif ++
%     show(counters[r,c]) 
%   | r in Racks, c in  Cards
% ]
;

