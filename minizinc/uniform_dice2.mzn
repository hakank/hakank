% 
% Uniform Dice in MiniZinc.
%
% From Mind Your Decisions
% """
% You are given a normal die and a blank die. (Each die is six-sided and
% equally likely to show each face).
% 
% Label the blank die using the numbers 0 to 6 so that when you roll the two die the
% sum shows each whole number from 1 to 12 with equal chance.
% 
% You can use a number more than once, or not at all, so you could label the
% faces 1, 2, 3, 4, 4, 5. But you do have to label all six faces of the blank die.
% """
%
% Solution (unique modulo symmetry):
% 
% z   : 12
% dist: [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]
% x1  : [1, 2, 3, 4, 5, 6]
% x2  : [0, 0, 0, 6, 6, 6]
%  1: 3
%  2: 3
%  3: 3
%  4: 3
%  5: 3
%  6: 3
%  7: 3
%  8: 3
%  9: 3
% 10: 3
% 11: 3
% 12: 3
% ----------
% ==========
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 

include "globals.mzn"; 
int: n = 6; % number of side of a die
int: m = 6; % max integer 


% the two dice
array[1..n] of int: x1 = [1,2,3,4,5,6];
array[1..n] of var 0..m: x2;
array[1..m*2] of var 0..n*n: dist;
var 0..m*2: z = sum(k in 1..m*2) (dist[k] > 0);


solve satisfy;


predicate same(array[int] of var int: x) =
   forall(k in 1..length(x)-1) (
       x[k] = x[k+1]
   )
;
        
constraint
  forall(k in 1..m*2) (
    dist[k] = sum(i,j in 1..n) ( x1[i]+x2[j] == k)
  )
  /\ same(dist)
  /\ z = 2*n % all elements in dist must be > 0

  % redundant constraints
  /\ sum(dist) = n*n
  
  % symmetry breaking
  /\ increasing(x2) 
 
;


output 
[
  "z   : " ++ show(z) ++ "\n" ++
  "dist: " ++ show(dist) ++ "\n" ++
  "x1  : " ++ show(x1) ++ "\n" ++
  "x2  : " ++ show(x2) ++ "\n" 
]
++
[
  if fix(dist[i]) > 0 then
     show_int(2, i) ++ ": " ++ show(dist[i]) ++ "\n"
  else 
    ""
  endif
  | i in 1..m*2
];
