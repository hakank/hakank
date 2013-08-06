% Coloring problem in Minizinc
%
%% From 
%% Katta G. Murty: "Optimization Models for Decision Making", page 344f
%% http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


% Some European countries
%       1         2           3         4          5            6
% {"Belgium", "Denmark", "France", "Germany", "Netherlands", "Luxembourg"};
set of 1..6: countries = 1..6 ; % the countries
int: n = card(countries);       % number of countries 
set of 1..n: colors = 1..n; % {"blue", "red", "yellow", "gray"};  % the colors
int: num_nodes = 9; % number of nodes
array[1..num_nodes,1..2] of int: graph; 
var 0..n: numColors;

% x: what color
array[countries] of var 1..n: x;


% minimize the number of colors
solve minimize numColors;
% solve satisfy;

constraint
   % No adjacent countries can have the same color
   forall(i in 1..num_nodes) (
      x[graph[i,1]] != x[graph[i,2]]
   )
   /\ % upper limit on the number of colors
   forall(i in countries) (
     x[i] <= numColors
   )
%    /\ numColors <= 4
;

%
% between two countries
%
graph = 
array2d(1..num_nodes, 1..2, [
 3, 1,
 3, 6,
 3, 4,
 6, 4,
 6, 1,
 1, 5,
 1, 4,
 4, 5,
 4, 2
]);


output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "numColors: " ++ show(numColors)
];