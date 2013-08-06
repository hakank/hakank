% 
% Map coloring with costs in MiniZinc.
% 
% From Tom Zaremba
% "Graph Coloring, Map Coloring, and Chromatic Number"
% http://www.geom.uiuc.edu/~zarembe/grapht2.html
% """
% You need to color the entire map of South America. This may 
% seem simple, but but there are some rescrictions.
%
%    - No country may touch another country of the same color.
%    - You will be charged each time you use a color to fill in a 
%      country -regardless of its size.
%    - You must color the map as cheaply as possible. 
% 
%    Color   Cost Per Country
%    Red     $100
%    Blue    $200
%    Green   $300
%    Orange  $400
%    Yellow  $500
%    Purple  $600
%    Brown   $700
%    Black   $800
% 
% The cost for each color is shown above.
% """

% The minimum cost is 3100 and there are 16 different solutions.
% Here is one solution:
%
%   total: 3100
%   x: [3, 2, 1, 2, 1, 2, 1, 3, 4, 1, 2, 2, 3, 1, 2, 1]
% 
%   Galapagos Islands: Green  300
%   Ecuador          : Blue   200
%   Colombia         : Red    100
%   Venezuela        : Blue   200
%   Guyana           : Red    100
%   Suriname         : Blue   200
%   French Guiana    : Red    100
%   Brazil           : Green  300
%   Peru             : Orange 400
%   Bolivia          : Red    100
%   Paraguay         : Blue   200
%   Chile            : Blue   200
%   Argentina        : Green  300
%   Uruguay          : Red    100
%   Falklands Island : Blue   200
%   South Georgia    : Red    100


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% Countries 
int: GalapagosIslands = 1;
int: Ecuador          = 2;
int: Colombia         = 3;
int: Venezuela        = 4;
int: Guyana           = 5;
int: Suriname         = 6;
int: FrenchGuiana     = 7;
int: Brazil           = 8;
int: Peru             = 9;
int: Bolivia          = 10;
int: Paraguay         = 11;
int: Chile            = 12;
int: Argentina        = 13;
int: Uruguay          = 14;
int: FalklandsIsland  = 15;
int: SouthGeorgia     = 16;

int: num_countries = 16;
array[1..num_countries] of set of int: neighbours = 
  [
  { Ecuador, Colombia},  % GalapagosIslands,
  { GalapagosIslands,Colombia,Peru},  % Ecuador
  { Venezuela, Ecuador, Peru, Brazil},  % Colombia
  { Colombia, Guyana, Brazil,},   % Venezuela
  { Venezuela, Suriname, Brazil},   % Guyana
  { Guyana, FrenchGuiana, Brazil},   % Suriname
  { Suriname, Brazil},   % FrenchGuiana
  { FrenchGuiana, Suriname, Guyana, Venezuela, Colombia, Peru, Bolivia, Paraguay, Uruguay},   % Brazil
  { GalapagosIslands, Ecuador, Colombia, Brazil, Bolivia, Chile},   % Peru
  { Peru, Brazil, Paraguay, Chile, Argentina},   % Bolivia
  { Bolivia, Brazil, Argentina, Uruguay},   % Paraguay
  { Peru, Bolivia, Argentina},   % Chile
  { Chile, Bolivia, Paraguay, Uruguay, FalklandsIsland},   % Argentina
  { Argentina, Paraguay, Brazil},   % Uruguay
  { Argentina, SouthGeorgia},   % FalklandsIsland
  { FalklandsIsland},   % SouthGeorgia
  ];

array[1..num_countries] of string: countries_str = [
   "Galapagos Islands",
   "Ecuador          ",
   "Colombia         ",
   "Venezuela        ",
   "Guyana           ",
   "Suriname         ",
   "French Guiana    ",
   "Brazil           ",
   "Peru             ",
   "Bolivia          ",
   "Paraguay         ",
   "Chile            ",
   "Argentina        ",
   "Uruguay          ",
   "Falklands Island ",
   "South Georgia    "
];

% Colors/Cost
% Red 	  $100
% Blue 	  $200
% Green   $300
% Orange  $400
% Yellow  $500
% Purple  $600
% Brown   $700
% Black   $800
int: num_colors = 8;
array[1..num_colors] of int: color_cost = [100,200,300,400,500,600,700,800];
array[1..num_colors] of string: color_str = 
   ["Red   ", 
    "Blue  ",
    "Green ",
    "Orange",
    "Yellow",
    "Purple",
    "Brown ",
    "Black "];

% Note: must be 4 colors (3 is not enough)
int: max_colors = 4;

% Assigned colors
array[1..num_countries] of var 1..num_countries: x;
var 0..num_countries*max(color_cost): total;

solve minimize total;
% solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) minimize total;

constraint
   forall(i in 1..num_countries, j in neighbours[i]) (
       x[i] != x[j]
   )
   /\
   total = sum(i in 1..num_countries) ( color_cost[x[i]] ) 
   /\
   forall(i in 1..num_countries) (
      x[i] <= max_colors
   ) 
;

% constraint total = 3100;

output [
 "total: " ++ show(total) ++ "\n" ++
 "x: " ++ show(x) ++ "\n\n"
] ++
[
  show(countries_str[i]) ++ ": " ++ color_str[fix(x[i])] ++ " " ++ show(color_cost[x[i]]) ++ "\n"
  | i in 1..num_countries
] 
++ ["\n"]
;

