% 
% Crystal Maze problem in MiniZinc.
% 
% From
% http://phys.org/news/2015-11-problems-ai-future-british-gameshow.html
% """
% On the wall was written a clue: "No consecutive letters in adjacent circles". 
% The letters A to H were printed on circular plates which could be fitted onto each circle.
% 
%       ?  ?
%   ?   A  H ?
%       ?  ? 
% 
%  [
% 
%    Represented with numbers, the cell are:
%     
%        2 3
%     1  4 5 6 
%        7 8
% 
%    And the connections:
%   
%     1,2,
%     1,4,
%     1,7,
%     2,3,
%     2,4,
%     2,5,
%     3,5,
%     3,6,
%     4,5,
%     4,7,
%     4,8,
%     5,6,
%     5,7,
%     5,8,
%     6,8,
%     7,8  
%  ]   
%
% """

% There are  four solutions:
%
%          D    F
%     G    A    H    B
%          C    E
%
% ----------
%
%          E    B
%     G    A    H    D
%          C    F
%
% ----------
%
%          C    E
%     G    A    H    B
%          D    F
%
% ----------
%
%          D    B
%     G    A    H    F
%          E    C
%
% ----------


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;
int: G = 7;
int: H = 8;

int: n = 8; % A .. H
int: num_connections = 16;
array[1..num_connections,1..2] of int: connections;

array[1..n] of int: hints;

array[1..n] of string: s = ["A","B","C","D","E","F","G","H"];

% decision variables
array[1..n] of var 1..n: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  all_different(x) /\

  forall(i in 1..num_connections) (
     abs(x[connections[i,1]] - x[connections[i,2]]) > 1
  )

  /\
  forall(i in 1..n) (
    if hints[i] > 0 then
       x[i] = hints[i]
    else
      true
    endif
  )

;

output [

 show(x), "\n",
 show([s[fix(x[i])] | i in 1..n]), "\n",
 
]
++
[
   "\n",
   "       \(x[2])    \(x[3])\n",
   "  \(x[1])    \(x[4])    \(x[5])    \(x[6])\n",
   "       \(x[7])    \(x[8])\n",
   "\n",
]
++
[
   "\n",
   "      \(s[fix(x[2])])    \(s[fix(x[3])])\n",
   " \(s[fix(x[1])])  \(s[fix(x[4])])    \(s[fix(x[5])])    \(s[fix(x[6])])\n",
   "      \(s[fix(x[7])])    \(s[fix(x[8])])\n",
   "\n",
]
;



connections = array2d(1..num_connections, 1..2,
[
  1,2,
  1,4,
  1,7,
  2,3,
  2,4,
  2,5,
  3,5,
  3,6,
  4,5,
  4,7,
  4,8,
  5,6,
  5,7,
  5,8,
  6,8,
  7,8  
]);

hints = [
   0, % 1
   0, % 2
   0, % 3
   A, % 4
   H, % 5
   0, % 6
   0, % 7
   0, % 8
  
]