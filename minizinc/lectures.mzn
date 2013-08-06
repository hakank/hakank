% 
% Lectures problem in MiniZinc.
%
% Biggs: Discrete Mathematics (2nd ed), page 187.
% """
% Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
% Among the the potential audience there are people who wish to hear both
%
%  - v1 and v2
%  - v1 and v4
%  - v3 and v5
%  - v2 and v6
%  - v4 and v5
%  - v5 and v6
%  - v1 and v6
%
% How many hours are necessary in order that the lectures can be given
% without clashes?
% """
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6;                    % number of nodes
int: edges = 7;                % number of edges

set of int: r = 1..n;          % the nodes
array[1..edges, 1..2] of r: g; % the graph

array[r] of var  r: v;

var r: max_c;

% solve satisfy;
solve minimize max_c; % minimize the number of colors, i.e. the largest color number

% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
%   max_c = 3
%   /\
   maximum(max_c, v)
   /\
   forall(i in 1..edges) (
      v[g[i,1]] != v[g[i,2]]
   )
   /\ % symmetry breaking: v1 has the color 1, v2 has either color 1 or 2
      % (this should be general enough for a general model)
   v[1] = 1
   /\
   v[2] <= 2

;

%
% The schedule requirements:
%     lecture a cannot be held at the same time as b
%
g = array2d(1..edges, 1..2, 
  [
   1, 2,
   1, 4,
   3, 5,
   2, 6,
   4, 5,
   5, 6,
   1, 6
  ]);

output [

  show(v), "\n"
];