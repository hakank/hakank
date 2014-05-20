% 
% Polygon in MiniZinc.
% 
% From 
% Jean Francois Puget: "Chebyshev Centers of Polygons in OPL"
% https://www.ibm.com/developerworks/community/blogs/jfp/entry/chebyshev_centers


% g12/mip and eplex solves this:
% [1.7465948633080317, 3.2369974924720717, 2.7465948633080317]

% ic give wrong solution, gecode shaves tiny slices and takes very long...


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 5;
array[1..n, 1..2] of int: vertices = 
  array2d(1..n, 1..2,
        [
        1,1,
        2,5,
        5,4,
        6,2,
        4,1
        ]);

% nextc is the circular version of next
% {edge} edges = {<v, nextc(vertices,v), nextc(vertices,v).x - v.x, nextc(vertices,v).y - v.y> | v in vertices};

array[1..n, 1..6] of int: edges = 
    array2d(1..n, 1..6,
          [
        1,1, 2,5,  2-1, 5-1,  
        2,5, 5,4,  5-2, 4-5,
        5,4, 6,2,  6-5, 2-4,
        6,2, 4,1,  4-6, 1-2,
        4,1, 1,1,  1-4, 1-1,
          ]);

var float: r;
var float: x;
var float: y;

% solve satisfy;
% solve :: float_search([x,y,r],0.01, input_order, indomain_split, complete) maximize r;
solve maximize r;

constraint
   r >= 0.0 /\
   x >= 0.0 /\
   y >= 0.0 /\
   forall(i in 1..n) (
        % <<x1, y1>, <x2, y2>, dx, dy> in edges
        % r*sqrt(dx^2 + dy^2) <= dy*(x - x1) - dx*(y - y1)
        let {
           float: x1 = int2float(edges[i,1]),
           float: y1 = int2float(edges[i,2]),
           float: x2 = int2float(edges[i,3]),
           float: y2 = int2float(edges[i,4]),
           float: dx = int2float(edges[i,5]),
           float: dy = int2float(edges[i,6]),
        } in
        r*sqrt(dx*dx + dy*dy) <= dy*(x - x1) - dx*(y - y1)
   )
;

output [
  show([r,x,y])
];

