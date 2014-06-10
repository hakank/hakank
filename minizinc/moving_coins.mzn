% 
% Moving coins problem in MiniZinc.
% 
% 
% 10 coins are ordered in a triangle pointed to the top.
% Arrange the coins in a triangle such that it is pointing down 
% by moving as few coins as possible.
%
%
% Start:
%         c 
%       c   c
%     c   c   c
%   c   c   c   c
%
% Goal:
%
%
%   c   c   c   c
%     c   c   c          
%       c   c
%         c
%
%
% Representation of the start position in
% a grid of 20 x 20  (so we can expand in any direction)
%
%
%   1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 9 0
%  1             
%  2
%  3
%  4
%  5
%  6
%  7
%  8
%  9 
% 10                  c 
% 11                c   c
% 12              c   c   c
% 13            c   c   c   c
% 14
% 15
% 16
% 17
% 18
% 19 
% 20

% This mode was inspired by the LPL model:
%   http://lpl.unifr.ch/lpl/Solver.jsp?name=/triangle
% but is more general.
% 
% Note that the problem instance is in term of k, the number of rows
% the rest is figured out along the way, e.g.
%   n = 1+2+..+k
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: k;        % number of rows
int: n;        % number of coins
int: n2 = n*2; % the grid

% array[1..n2,1..n2] of int: grid;

% The start positions
array[1..n,1..2] of int: start_positions2 = 
array2d(1..n,1..2,
  [
    if p = 1 then n+a-1 else n+a-b+1  endif
    | a in 1..k, b in 2..a*2, p in 1.. 2 where b mod 2 = 0
  ]);
% The problem instance
array[1..n2,1..n2] of var 0..1: grid2;


% decicion variables
array[1..n2,1..n2] of var 0..1: x;
var 1..n2*n2: z = sum([bool2int(x[i,j] > 0 /\ x[i,j] = grid2[i,j]) | i,j in 1..n2]);

solve maximize z;
% solve :: int_search([x[i,j] | i,j in 1..n2], first_fail, indomain_min, complete) maximize z;

constraint

  % create the problem instance from the start positions
  forall(i in 1..n2, j in 1..n2) (
    if exists(s in 1..n) ( start_positions2[s,1] = i /\ start_positions2[s,2] = j) then
        grid2[i,j] = 1
     else
        grid2[i,j] = 0
     endif
  )

  % /\ % same number of coins (doesn't help much, and make flattening slower)
  % sum([x[i,j] | i,j in 1..n2 ]) = sum([grid2[i,j] | i,j in 1..n2 ])


  /\ % stronger: same number of coins in each column
  forall(j in 1..n2) (
    sum([x[i,j] | i in 1..n2 ]) = sum([grid2[i,j] | i in 1..n2 ])
  )
  
  % /\ % place the new triangle (10 coins), explicit approach
  % exists(i,j in 1..n2) (
  %    x[i-3,j-3] = 1 /\ x[i-3,j-1] = 1 /\ x[i-3,j+1] = 1 /\ x[i-3,j+3] = 1 /\ % 4 coins  
  %         x[i-2,j-2] = 1 /\ x[i-2,j] = 1 /\ x[i-2,j+2] = 1 /\                % 3 coins
  %                x[i-1,j-1] = 1 /\ x[i-1,j+1] = 1 /\                         % 2 coins
  %                           x[i,j] = 1                                       % 1 coin
  % ) 

  /\ % place the coins properly, general approach
  exists(i,j in 1..n2) (
     forall(a in 1..k) (
       forall(b in 2..a*2 where b mod 2 = 0) (
          x[i-a,j+a-b] = 1
       )
     )
  )

  % somewhat cheating: the number of moves is n div 3
  % /\ z = n-(n div 3)

;

output 
[
  "z: " ++ show(z) ++ "\n",
  "Start positions:"
]
++
[
  if j = 1 then "\n" else " " endif ++
     show(grid2[i,j])
  |i,j in 1..n2
] 
++
[
 "\nResult positions:"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n2
]
++
[ 
 "\nThe moves:\nC: Coin at the same place. R: removed coin. N: new place of a coin."
]
++
[
  if j = 1 then "\n" else " " endif ++
    if fix(grid2[i,j]) = 1 /\ fix(x[i,j]) = 0 then
      "R"
    else 
      if fix(x[i,j]) = 1 then
        if fix(grid2[i,j]) = 0 then
          "N"
        else  
          "C"
        endif
      else
        "_"  
      endif
    endif

  | i,j in 1..n2
]
++
[
  "\nThus, we need to move n (" ++ show(n) ++ ") - z (" ++ show(z) ++ ") = " ++ show(n-fix(z)) ++ " coins.\n" ++
  "n: " ++ show(n) ++ " n2: " ++ show(n2) ++ "\n",
  "z: " ++ show(z) ++ "\n",
  "(By theory, we need to move n div 3 coins, i.e. " ++ show(n div 3) ++ " coins.)\n",
]
;


% original problem
% n = 10;
% k = 4; % number of rows

k = 7; % number of rows
n = sum([i | i in 1..k]);


% original positions of the coins (k=4)
% grid = array2d(1..n2,1..n2,
%   [
% %             v v v v v v v
% % 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 1
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 2
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 3
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 4
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 5
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 6
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 7
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 8
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 9
%   0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0, % 10  <-
%   0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0, % 11  <-
%   0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0, % 12  <-
%   0,0,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0, % 13  <- 
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 14
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 15
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 16
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 17
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 18
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 19
%   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 20
%   ]);

%% start positions for k =4
% array[1..n,1..2] of int: start_positions = 
%   array2d(1..n, 1..2,
%   [
%             10,10,
%          11,9, 11,11,
%        12,8, 12,10, 12,12,
%       13,7, 13,9, 13,11, 13,13
%   ]);
