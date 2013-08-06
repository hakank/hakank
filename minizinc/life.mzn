% 
% Life (simple) in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
% """
%  1. Any live cell with fewer than two live neighbours dies, as if by loneliness.
%  2. Any live cell with more than three live neighbours dies, as if by overcrowding.
%  3. Any live cell with two or three live neighbours lives, unchanged, 
%     to the next generation.
%  4. Any dead cell with exactly three live neighbours comes to life.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


int: n;
int: m;
array[1..n, 1..n] of 0..1: start; % start position
array[1..m, 1..n, 1..n] of var 0..1: x; % the evolutions


%
% life(from, to, n)
% (8 neighbours)
%
predicate life(array[int, int] of var 0..1: s, array[int,int] of var 0..1: t, int: nn) =
  let {
     array[1..nn, 1..nn] of var 0..nn: neigh
  }
  in
  % calculate the neighbours
  forall(i,j in 1..n) (
     neigh[i,j] = sum( a,b in {-1, 0, 1} where 
          i+a > 0  /\ j+b >  0 /\
          i+a <= nn /\ j+b <= nn
       ) (
        s[i+a,j+b]
     ) -  s[i,j]
  )

  /\ % calculate the life of the cells
  forall(i,j in 1..nn) (
    (
     (s[i,j] = 1 /\ (neigh[i,j] = 3 \/ neigh[i,j] = 2))
     \/
     (s[i,j] = 0 /\ neigh[i,j] = 3)
    )
    <-> t[i,j] = 1    
  )
;

% solve satisfy;
solve :: int_search([x[k,i,j] | k in 1..m, i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint

   % initialize
   forall(i, j in 1..n) ( 
      x[1,i,j] = start[i,j]
   )
   /\ % evolve
   forall(k in 2..m) (
      life(array2d(1..n, 1..n, [x[k-1,i,j] | i,j in 1..n]), array2d(1..n, 1..n, [x[k,i,j] | i,j in 1..n]), n)
   )
;

%
% data
%

% blinker
%n = 8;
%m = 20;
%start = array2d(1..n, 1..n, 
%        [ 
%           0,0,0,0,0,0,0,0,
%           0,0,0,0,0,0,0,0,
%           0,0,1,1,1,0,0,0,
%           0,0,0,0,0,0,0,0,
%           0,0,0,0,0,0,0,0,
%           0,0,0,0,0,0,0,0,
%           0,0,0,0,0,0,0,0,
%           0,0,0,0,0,0,0,0,
%         ]);

% glider
n = 8;
m = 20;
start = array2d(1..n, 1..n, 
        [ 
           0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,
           0,0,0,0,0,1,1,1,
           0,0,0,0,0,1,0,0,
           0,0,0,0,0,0,1,0,
         ]);

% toad
%n = 8;
%m = 20;
% start = array2d(1..n, 1..n, 
%         [ 
%            0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,
%            0,0,1,1,1,0,0,0,
%            0,1,1,1,0,0,0,0,
%            0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,
%          ]);


% glider on 16x16
% n = 16;
% m = 30;
% start = array2d(1..n, 1..n, 
%         [ 
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
%            0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
%          ]);


output [
  if i = 1 /\ j = 1 then "\n" else "" endif ++
   if j = 1 then "\n" else " "  endif ++
     show(x[k,i,j])
   | k in 1..m, i,j in 1..n
] ++ ["\n"];


