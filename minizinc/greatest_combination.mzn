% 
% Greatest combination problem in MiniZinc.
% 
% From Stack Overflow:
% solving prolog puzzle
% http://stackoverflow.com/questions/10001344/solving-prolog-puzzle
% """
% I am trying to solve a puzzle in prolog that involves taking a square 
% of numbers (a list of a list of numbers) and returning the list of the 
% greatest combination of numbers starting at the top and going down, 
% row by row. Each move must be either down, down to the right, or 
% down to the left.
%
% I've been trying to do this for a while now, does anyone have a 
% place I could begin?
%
% for example:
%
% on the board [[0,2,1,0], [0,1,1,0], [0,10,20,30]]
%
% the best move would be [1,2,3] for 33 points.
% 
% """

% Notes: 
%  - the example use 0-based index
%  - there are two optimal solutions (using 1-based) with
%    values of 33:
%    * 2,2,4  (values: 2, 1, 30)
%    * 2,3,4  (values: 2, 1, 30)  This is the solution in the example.
%


% Another example (from the comments):
% """
% if the square is like this [0,1,1] [0,2,1] [10,0,0] the program should say 
% that the best path is [1,1,0] for 13 points. the 1st element in the first 
% line, 1st element in the 2nd and 0th element in the third. 
% """

% There are two optional solutions (with 13 points):
%   * 2,2,1   (values 1, 2, 10)
%   * 3,2,1   (values 1, 2, 10)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 3; % rows
int: m = 4; % columns % first example
% int: m = 3; % columns % second example

array[1..n, 1..m] of int: grid;

% decision variables
array[1..n] of var 1..m: x;

var 0..sum(grid): max_val = sum(i in 1..n) (grid[i, x[i]]);

% solve satisfy;
solve maximize max_val;
% solve :: int_search(x, first_fail, indomain_max, complete) maximize max_val;

% constraint
%    forall(i in 2..n) (
%      exists(j in 1..m) (
%          abs(x[i-1]-j) <=1
%          /\
%          x[i] = j
%      ) 
%    )
% ;

% 
constraint
   forall(i in 2..n) (
     let {
       var 1..m: j
     } in
     abs(x[i-1]-j) <=1
     /\
     x[i] = j
   )
;


% constraint max_val = 33;  % first example
% constraint max_val = 13;  % second example

output [
  "max_val: " ++ show(max_val) ++ "\n" ++
  "x      : " ++ show(x) ++ "\n" ++ 
  "values : " ++ show([grid[i,fix(x[i])] | i in 1..n]) ++ "\n"
]
 ++ ["\n"]
;


%
% data
%
grid = array2d(1..n, 1..m,
[ 0, 2, 1, 0,
  0, 1, 1, 0,
  0,10,20,30
]);

% grid = array2d(1..n, 1..m,
% [
%    0, 1, 1, 
%    0, 2, 1,
%   10, 0, 0
% ]);
