% 
% Furniture offer optimization in MiniZinc.
% 
% From
% https://twitter.com/MrVaudrey/status/725510805471416320
% via @iamreddave's tweet to me: https://twitter.com/iamreddave/status/725808714423361536
%
% @MrVauder tweeted a picture of different offers for buying furnitures with the comment
% "This furniture store just served me up a sytem of equations"
% 
% The different offers are to buy a table plus these combinations:
%  - 2 arm chairs + 4 side chairs           for 1599.99
%  - 2 arm chairs + 2 side chairs + 1 bench for 1599.99
%  -                8 side chairs           for 1859.99
%  -                6 side chairs + 1 bench for 1859.99
%  - 2 arm chairs + 6 side chairs           for 1899.99
%  - 2 arm chairs + 8 side chairs           for 2099.99
%  - 2 arm chairs + 6 side chairs + 1 bench for 2099.99
% 
%
% I interpret the problem as to create the best cominations of offers to buy 
% at least some amount of each furnitures (A ArmChairs + S SideChairs + B Benches).
%
% Note: I translated the float prices for the offers to integers since few
% FlatZinc solvers support floats (but see below for a result with Gecode).
%
%
% Example: We want (at least) 12 ArmChairs, 8 SideChairs and 1 Bench
% and also minimize the total cost.
% 
% The optimal solutions is a total of 9600
% -> We should buy 5 of offer 1 and 1 of offer 2 for a total of 9600
% (and for this we got 12 ArmChairs, 22 ArmChairs and 1 Bench)
%  
%   prefs: [12, 8, 1]
%   Total: 9600
%   to_buy: [5, 1, 0, 0, 0, 0, 0]
%
%   What we got:
%   ArmChairs: 12
%   SideChairs: 22
%   Benches: 1
% 
% There are actually 6 optimal solutions:
%
%   to_buy: [5, 1, 0, 0, 0, 0, 0]
%   to_buy: [4, 2, 0, 0, 0, 0, 0]
%   to_buy: [3, 3, 0, 0, 0, 0, 0]
%   to_buy: [2, 4, 0, 0, 0, 0, 0]
%   to_buy: [1, 5, 0, 0, 0, 0, 0]
%   to_buy: [0, 6, 0, 0, 0, 0, 0]
%

%
% Here's the result when using floats with Gecode (it's same result 
% except for the total):
%
%   prefs: [12, 8, 1]
%   Total: 9599.940000000001
%   to_buy: [5, 1, 0, 0, 0, 0, 0]
%
%   What we got:
%   ArmChairs: 12
%   SideChairs: 22
%   Benches: 1


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 


int: num_offers = 7;
%% Note: We use integers instead of float (1599.99 -> 1600) since there are few
%%       FlatZinc solver with support for float variables.
% array[1..num_offers] of float: offer_price = [1599.99,1599.99,1859.99,1859.99,1899.99,2099.99,2099.99]; % float
array[1..num_offers] of int: offer_price = [1600,1600,1860,1860,1900,2100,2100]; % int

int: num_products = 3;
array[1..num_products] of  string: furnitures_s = ["ArmChairs","SideChairs","Benches"];

% Amount of each product in the separate offers.
array[1..num_offers, 1..num_products] of int: mat =
array2d(1..num_offers, 1..num_products,
[
  % ArmChair  SideChair  Bench
    2, 4, 0,  % offer 1
    2, 2, 1,  % offer 2
    0, 8, 0,  % offer 3
    0, 6, 1,  % offer 4
    2, 6, 0,  % offer 5
    2, 8, 0,  % offer 6
    2, 6, 1,  % offer 7
]);

% We want to buy (at least)
%   prefs[1]*ArmChairs
%   prefs[2]*SideChairs 
%   prefs[3]*Benches  
%                                    %   A  S  B 
array[1..num_products] of int: prefs = [12, 8, 1];
%% Some other tests
% array[1..num_products] of int: prefs = [12, 8, 2]; % another test
% array[1..num_products] of int: prefs = [3, 8, 2]; % another test
% array[1..num_products] of int: prefs = [9, 8, 2]; % another test

% decision variables
array[1..num_offers] of var 0..9999: to_buy;

% var float: Total = sum([offer_price[i]*to_buy[i] | i in 1..num_offers]); % float
var int: Total = sum([offer_price[i]*to_buy[i] | i in 1..num_offers]); % int

solve minimize Total;
% solve satisfy;

constraint
  % ensure that all prefered products are bought
  forall(j in 1..num_products) (
    sum([to_buy[i]*mat[i,j] | i in 1..num_offers]) >= prefs[j]
  )

  % /\ Total = 9600 % for solve satisfy on prefs = [12, 8, 1]

;

output [
  "prefs: \(prefs)\n",
  "Total: \(Total)\n",
  "to_buy: \(to_buy)\n",
]
++ ["\nWhat we got:\n" ]
++
[
  "\(furnitures_s[j]): " ++ show(sum([to_buy[i]*mat[i,j] | i in 1..num_offers])) ++ "\n"
  | j in 1..num_products
]
;

