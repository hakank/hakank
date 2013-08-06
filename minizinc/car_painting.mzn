% 
% Car painting in MiniZinc.
% 
% From SAS/OR 9.22 User's Guide: Constraint Programming
% Car Painting Problem
% http://support.sas.com/documentation/cdl/en/orcpug/63349/HTML/default/viewer.htm#orcpug_clp_sect039.htm
% """
% Example 3.5 Car Painting Problem
%%
% The car painting process is an important part of the automobile manufacturing industry. 
% Purging (the act of changing colors in the assembly process) is expensive due to the 
% added cost of wasted paint and solvents involved with each color change in addtion (sic!) 
% to the extra time required for the purging process. The objective in the car painting 
% problem is to sequence the cars in the assembly in order to minimize paint changeover 
% (Sokol; 2002; Trick; 2004).
% 
% There are 10 cars in a sequence. The order for assembly is 1, 2, ..., 10. A car must 
% be painted within three positions of its assembly order. For instance, car 5 can be painted 
% in positions 2 through 8 inclusive. Cars 1, 5, and 9 are red; 2, 6, and 10 are blue; 
% 3 and 7 green; and 4 and 8 are yellow. The initial sequence 1, 2, ..., 10 corresponds 
% to the color pattern RBGYRBGYRB and has 9 purgings. The objective is to find a solution 
% that minimizes the number of purgings.
% 
% This problem can be formulated as a CSP as follows. The variables and represent the 
% ID and color, respectively, of the car in slot . An element constraint relates the car 
% ID to its color. An all-different constraint ensures that every slot is associated with 
% a unique car ID. Two linear constraints represent the constraint that a car must be painted 
% within three positions of its assembly order. The binary variable indicates whether a paint 
% purge takes place after the car in slot is painted. Finally, a linear constraint is used to 
% limit the total number of purgings to the required number. 
% 
% References:
% Sokol, J. (2002), Modeling Automobile Paint Blocking: A Time Window Traveling Salesman Problem, 
%                   Ph.D. thesis, Massachusetts Institute of Technology, Cambridge, MA. 
% Trick, M. (2004), “Constraint Programming: A Tutorial,” http://mat.gsia.cmu.edu/trick/cp.ppt. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 10;

int: red = 1;
int: blue = 2;
int: green = 3;
int: yellow = 4;

% colors of the original positions
%                              1    2     3      4       5    6     7      8       9   10
array[1..n] of 1..4: colors = [red, blue, green, yellow, red, blue, green, yellow, red, blue];
array[1..4] of string: col_str = ["red", "blue","green", "yellow"];


% decision variables
array[1..n] of var 1..n: x; % which place
var 0..n: num_purgings; % number of purgings
var 0..n*n: distance; % total distance of moved positions

% solve minimize num_purgings;
% solve satisfy;
solve :: int_search(
        x, 
        most_constrained, 
        indomain_split, 
        complete) 
    minimize num_purgings;
    % minimize distance; % note: need a fixed num_purgings
    % satisfy;

constraint
   % a card can be painted once
   alldifferent(x)

   % must be within 3 positions of the original position
   /\ 
   forall(i in 1..n) (
      abs(x[i]-i) <= 3
   )

   % how many purgings?
   /\ 
   num_purgings = sum(i in 2..n) ( bool2int(colors[x[i]] != colors[x[i-1]]) )

   % total distance of changed positions
   /\ 
   distance = sum(i in 1..n) ( abs(x[i] - i) )

   % minimum number of purgings are 5
   % /\ num_purgings <= 5 

   % symmetry breaking
   % x[1] = red
;

output [
  "num_purgings: " ++ show(num_purgings) ++ "\n" ++
  "distance    : " ++ show(distance) ++ "\n" ++
  "x           : " ++ show(x) ++ "\n" ++
  "x(colors)   : "
] ++
[
  show(col_str[colors[fix(x[i])]]) ++ " "
  | i in 1..n
]
++ ["\n"]
;

