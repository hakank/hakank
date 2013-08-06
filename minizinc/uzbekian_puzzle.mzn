% 
% Uzbekian Puzzle in MiniZinc.
% 
% From
% Belaid MOA, "Examples for Discrete Constraint Programming"
% http://www.docstoc.com/docs/23988870/Examples-for-Discrete-Constraint-Programming
% page 49f
% """
% An Uzbekian sales man met five traders who live in five
% different cities. The five traders are:
%   {Abdulhamid, Kurban, Ruza,Sharaf, Usman}
% The five cities are:
%   {Bukhara, Fergana, Kokand, Samarkand, Tashkent}
% Find the order in which he visited the cities given the 
% following information.
%  - He met Ruza before Sharaf after visiting Samarkand,
%  - He reached Fergana after visiting Samarkand followed by
%    two other cities
%  - The third trader he med was Tashkent
%  - Immediately after his visit to Bukhara, he met Abdulhamid
%  - He reached Kokand after visiting the city of Kurban followed
%    by two other cities.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: Bukhara   = 1;
int: Fergana   = 2;
int: Kokand    = 3;
int: Samarkand = 4;
int: Tashkent  = 5;
int: NumCities = 5;
array[1..NumCities] of var 1..NumCities: vstCty;

int: Abdulhamid = 1;
int: Kurban     = 2;
int: Ruza       = 3;
int: Sharaf     = 4;
int: Usman      = 5;
int: NumTraders = 5;
array[1..NumTraders] of var 1..NumTraders: mtTrader;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   alldifferent(mtTrader)                     /\
   alldifferent(vstCty)                      /\

   mtTrader[Ruza] < mtTrader[Sharaf]          /\
   mtTrader[Ruza] > vstCty[Samarkand]         /\
   vstCty[Fergana] = vstCty[Samarkand] + 2    /\
   vstCty[Tashkent] = 3                       /\
   mtTrader[Abdulhamid] = vstCty[Bukhara] + 1 /\
   vstCty[Kokand] = mtTrader[Kurban] + 2
;

output
[
  "vstCty: " ++ show(vstCty) ++ "\n" ++
  "mtTrader: " ++ show(mtTrader) ++ "\n"
];