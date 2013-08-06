%
% Simple optimizing problem in ASP.
%  
% Minimize the cost for the products:
% Type of                        Calories   Chocolate    Sugar    Fat
% Food                                      (ounces)     (ounces) (ounces)
% Chocolate Cake (1 slice)       400           3            2      2
% Chocolate ice cream (1 scoop)  200           2            2      4
% Cola (1 bottle)                150           0            4      1
% Pineapple cheesecake (1 piece) 500           0            4      5
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

%
% Solution should be
% 
%   total_cost: 90
%   chocolate_cake: 0
%   chocolate_ice_cream: 3
%   cola: 1
%   pineapple_cheesecake: 0
%

%
% Data:
%
#const n = 10.
amount(0..n). % max amount of each product

%    food                  calories  chocolate   sugar     fat    price
%                          (ounces)    (ounces) (ounces) (ounces) 
food_a(chocolate_cake,       400,      3,          2,        2,   50).
food_a(chocolate_ice_cream,  200,      2,          2,        4,   20).
food_a(cola,                 150,      0,          4,        1,   30).
food_a(pineapple_cheesecake, 500,      0,          4,        5,   80).

% Minimum limits
limits(calories, 500).
limits(chocolate, 6).
limits(sugar, 10).
limits(fat, 8).

% extract the different nutrition types
calories(Food, Amount)  :- food_a(Food, Amount, _, _, _, _).
chocolate(Food, Amount) :- food_a(Food, _, Amount, _, _, _).
sugar(Food, Amount)     :- food_a(Food, _, _, Amount, _, _).
fat(Food, Amount)       :- food_a(Food, _, _, _, Amount, _).

% extrace the price
price(Food, Price) :- food_a(Food, _, _, _, _, Price).
prices(Price) :- price(Food, Price).

% food(chocolate_cake;chocolate_ice_cream;cola;pineapple_cheesecake).
food(F) :- food_a(F, _,_, _, _, _). 

% each food has exactly one Price and one Amount
1 { food_price_amount(Food,Price,Amount) : amount(Amount) } 1 :- 
                                                          food(Food),
                                                          price(Food, Price).

% sligthly more general
% Note that we must subtract the limit by 1 since the limits are
% (stated as a positive constraint):
%    sum(...) >= L
% Thus the integrity constraint is:
%   :- sum(...) < L-1
:- #sum[food_price_amount(F, _, A)  : calories(F, C)  = C*A] L-1, limits(calories, L).
:- #sum[food_price_amount(F, _, A)  : chocolate(F, C) = C*A] L-1, limits(chocolate, L).
:- #sum[food_price_amount(F, _, A)  : sugar(F, C)     = C*A] L-1, limits(sugar, L).
:- #sum[food_price_amount(F, _, A)  : fat(F, C)       = C*A] L-1, limits(fat, L).

% Objective: Minimize total cost
#minimize [food_price_amount(F,Price,Amount) = Price*Amount ].


#hide.
#show food_price_amount(F,P,A).


