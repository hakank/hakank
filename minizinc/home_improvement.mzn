% 
% Home improvement (PuzzlOR problem) in MiniZinc.
% 
% From INFORMS, PuzzlOR 
% http://www.informs.org/ORMS-Today/Public-Articles/October-Volume-37-Number-5/PUZZLOR
% """
% By John Toczek
%
% Preparing a house for sale may include making home improvements in order to 
% attract buyers and increase the sales price. Some home improvements add profit 
% to the sale of the house but most are not worth the cost.
% 
% Table 1 lists recently sold houses in your neighborhood. The selling price and 
% any improvements the house had at the time of sale are shown. For example, the 
% first house in the table sold for $181,000 and had a new kitchen, wood floors and new paint.
% 
% Your house, which you are about to put up for sale, has none of the five listed 
% improvements. You wish to identify the one improvement that will add the most profit to 
% the sale of your house. 
% Installation costs for the improvements are as follows: 
%   new bathroom – $10,000; 
%   new kitchen – $15,000; 
%   pool – $30,000; 
%   wood floors – $5,000; 
%   new paint – $1,000.
% 
% Assume that, other than the five improvements, your house is very similar 
% to the houses listed in Table 1 (sqft, land area, # bedrooms, #bathrooms, etc.) 
% and assume all improvements are of the same quality.
%
% [Table 1
% 
%   House      New       New       Pool   Wood       New
%   Selling    bathroom  kitchen          Floors     paint
%   Price
%  ---------------------------------------------------------
%  181000                  Y                Y         Y
%  213000       Y          Y        Y       Y
%  176000                  Y                          Y
%  169000                  Y                          Y
%  191000       Y                   Y                 Y
%  187000       Y                   Y                 Y
%  199000                  Y        Y                 Y
%  201000                  Y        Y       Y
%  200000                  Y        Y 
%
% ]

% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: num_houses = 9;
int: num_improvements = 5;

array[1..num_houses] of int: house_price = 
   [181000,213000,176000,169000,191000,187000,199000,201000,200000];
 
array[1..num_houses, 1..num_improvements] of 0..1: t = 
 array2d(1..num_houses, 1..num_improvements,
 [
  0, 1, 0, 1, 1,
  1, 1, 1, 1, 0,
  0, 1, 0, 0, 1,
  0, 1, 0, 0, 1,
  1, 0, 1, 0, 1,
  1, 0, 1, 0, 1,
  0, 1, 1, 0, 1,
  0, 1, 1, 1, 0,
  0, 1, 1, 0, 0
 ]);

array[1..num_improvements] of int: installation_cost = [10000,15000,30000, 5000,1000];

array[1..num_improvements] of var 0..200000: x;
array[1..num_houses, 1..num_improvements] of var -11000..11000: rest;
var -1000..1000: sum_rest = sum([rest[i,j] | i in 1..num_houses, j in 1..num_improvements]);
var 0..10000: max_rest = max([rest[i,j] | i in 1..num_houses, j in 1..num_improvements]);

% solve satisfy;
% solve minimize sum_rest;
solve :: int_search(
        x ++ [rest[i,j] | i in 1..num_houses, j in 1..num_improvements], 
        max_regret, 
        indomain_median, 
        complete) 
    minimize sum_rest;
    % minimize max_rest;

constraint
   forall(i in 1..num_houses) (
      let {
        var 0..240000: s = sum(j in 1..num_improvements) (t[i,j]*(x[j]-installation_cost[j]+rest[i,j]))
      } 
      in
        s = house_price[i]
   )
;

output [
   "x:" ++ show(x) ++ "\n" ++
   "sum_rest: " ++ show(sum_rest) ++ "\n" ++
   "max: " ++ show(max_rest)
] ++ 
[ 
  if j = 1 then "\n" else " " endif ++
   show(rest[i,j]) ++ " "
  | i in 1..num_houses, j in 1..num_improvements
] ++

["\n"];