% 
% Global constraint assign_and_counts in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cassign_and_counts.html
% """
% Given several items (each of them having a specific colour that may not 
% be initially fixed), and different bins, assign each item to a bin, 
% so that the total number n of items of colour COLOURS in each bin satisfies 
% the condition n RELOP LIMIT.
% 
% Example
% (
% <4>,
%    <
%     bin-1	colour-4,
%     bin-3	colour-4,
%     bin-1	colour-4,
%     bin-1	colour-5
%     >, <=, 2
% )
%
% Figure 4.23.1 shows the solution associated with the example. The items 
% and the bins are respectively represented by little squares and by the 
% different columns. Each little square contains the value of the key 
% attribute of the item to which it corresponds. The items for which the 
% colour attribute is equal to 4 are located under the thick line. 
% The assign_and_counts constraint holds since for each used bin 
% (i.e., namely bins 1 and 3) the number of assigned items for which the 
% colour attribute is equal to 4 is less than or equal to the limit 2.
% """

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 4;
int: num_colors = 5;
array[1..n] of var 1..n: bins;
array[1..n] of var 1..num_colors: colors;
var int: color;
var int: nval;

%
% Since MiniZinc don't handle function variables we use the following 
% hack where t is the type of comparison operator.
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
% else : a != b
%
predicate cmp(var int: a, var int: b, -2..2: t) = 
   if     t = -2 then
     a < b
   elseif t = -1 then
     a <= b 
   elseif t =  0 then
     a = b
   elseif t =  1 then
     a >= b
   elseif t =  2 then
     a > b
   else 
     a != b
   endif
;

predicate assign_and_counts(var int: color, array[int] of var int: bins, array[int] of var int: colors, int: relop, var int: nval) =

    forall(b in index_set(bins)) (
        let {
           var int: t_val
        }
        in
        t_val = sum(c in index_set(colors)) (
            bool2int(bins[c] = b /\ colors[c] = color)
        ) 
        /\ cmp(t_val,nval, relop)
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve satisfy;


output 
[ 
  "bins: ", show(bins), "\n",
  "colors: ", show(colors), "\n",
  "color: ", show(color), "\n",
  "nval: ", show(nval), "\n",
];

constraint

   % cp1d([1,1,1,1], bins) % the constraint do not hold here since there are 
                      % 3 occurrences of color 4 for bin 1, which is not 
                      % <= 2
   cp1d([1,3,1,1], bins) % the constraint holds
   /\
   cp1d([4,4,4,5], colors)
   /\
   color = 4
   /\
   nval = 2
   /\
   assign_and_counts(color, bins, colors, -1, nval)

;

