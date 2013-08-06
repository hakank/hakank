%
% Photo problem in Minizinc.
% 
% Problem statement from Mozart/Oz tutorial:
% http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
% """
% Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for taking a photo. Some of them have preferences next to whom they want to stand:
%
%    1. Betty wants to stand next to Gary and Mary.
%    2. Chris wants to stand next to Betty and Gary.
%    3. Fred wants to stand next to Mary and Donald.
%    4. Paul wants to stand next to Fred and Donald.
%
% Obviously, it is impossible to satisfy all preferences. Can you find an alignment that maximizes the number of satisfied preferences?
% """
% Oz solution: 
%   6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
% [5, 6, 1, 3, 7, 4, 2]
% 
% 
% (Cf the model in minizinc/examples/photo.mzn, which uses another model.)
%
%
% fz and ecl gives the following 8 solutions:
%              B  C  D  F  G  M  P
% positions = [3, 1, 6, 5, 2, 4, 7]
% positions = [3, 1, 7, 5, 2, 4, 6]
% positions = [3, 2, 6, 5, 1, 4, 7]
% positions = [3, 2, 7, 5, 1, 4, 6]
% positions = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
% positions = [5, 6, 2, 3, 7, 4, 1]
% positions = [5, 7, 1, 3, 6, 4, 2]
% positions = [5, 7, 2, 3, 6, 4, 1]
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

int: n = 7; % number of persons
array[1..n, 1..n] of 0..1: preferences;

array[1..n] of var 1..n: positions;

var int: z; 

solve :: int_search(positions, first_fail, indomain_min, complete) maximize z;
% solve :: int_search(positions, first_fail, indomain_min, complete) satisfy;

% helper predicate
predicate nextTo(var int: a, var int: b) = 
   abs(a - b) = 1 
   % \/ % for a circular collection, fisheye photo? :-)
   % ( (a = 1 /\ b = n) \/ (a = n /\ b = 1)  )
; 


constraint 
  all_different(positions) 
  /\
  z = sum(i,j in 1..n where preferences[i,j] > 0) ( 
        bool2int(nextTo(positions[i], positions[j]))
      )
  % /\  % for solve satisfy
  % z >= 6
;


% 1. Betty wants to stand next to Gary and Mary.
%     1 : 5, 6
% 2. Chris wants to stand next to Betty and Gary.
%     2 : 1, 5
% 3. Fred wants to stand next to Mary and Donald.
%     4 : 6, 3
% 4. Paul wants to stand next to Fred and Donald.
%     7 : 4, 3  

preferences = array2d(1..n, 1..n, [
% 1 2 3 4 5 6 7            
% B C D F G M P
  0,0,0,0,1,1,0, % Betty  1
  1,0,0,0,1,0,0, % Chris  2
  0,0,0,0,0,0,0, % Donald 3
  0,0,1,0,0,1,0, % Fred   4
  0,0,0,0,0,0,0, % Gary   5
  0,0,0,0,0,0,0, % Mary   6
  0,0,1,1,0,0,0, % Paul   7
])
;


output
[
  "positions: " ++ show(positions) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];