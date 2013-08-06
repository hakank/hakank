%
% Tripuzzle in MiniZinc
%

% Problem and OPL model in
% Martin J Chlond "Tri-Puzzle: A Three-Cornered Conundrum"
% http://ite.pubs.informs.org/Vol6No1/Chlond/index.php
% 
% Note in the original OPL model:
% 
% Model name   : tripuzzle.mod
% Author       : M J Chlond
% Description  : tri-puzzle
% Source       : Lagoon Games
% Date written : 27/8/05 

%
% MiniZinc model created by 
% Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: trin = 16;
int: posn = 16;
int: orient = 3;
int: spot = 3;  
int: color = 6;
  
set of 1..trin: T = 1..trin;
set of 1..posn: P = 1..posn;
set of 1..orient: O = 1..orient;
set of 1..spot: S = 1..spot;

array[P,O,S] of int: piece_col;
                
array[T,P,O] of var 0..1: x;
array[P,S] of var int: y; % >= 0

solve satisfy;

constraint
  forall(i in P, j in S) (
     y[i,j] >= 0
  )   
/\
  % each position occupied by one triangle in one orientation
forall(j in P) (
    sum(i in T, k in O) (x[i,j,k]) = 1
)
/\
  % each triangle in one position and one orientation
  forall(i in T) (
    sum(j in P, k in O) (x[i,j,k]) = 1
  )
/\
 
  % spot colours consistent with piece positions and orientations  
  forall(i in T, j in P, k in O, m in S) (
    y[j,m] >= piece_col[i,k,m] - color*(1-x[i,j,k]) /\
    y[j,m] <= piece_col[i,k,m] + color*(1-x[i,j,k])
  )
/\
  
  % color matches (outer)
  y[1,1] = 2 /\ 
  y[2,1] = 2 /\ 
  y[5,1] = 3 /\ 
  y[10,1] = 2 /\ 
  
  y[1,2] = 6 /\ 
  y[4,2] = 3 /\ 
  y[9,2] = 1 /\ 
  y[16,2] = 3 /\ 
  
  y[10,3] = 5 /\ 
  y[12,3] = 2 /\ 
  y[14,3] = 1 /\ 
  y[16,3] = 4 /\ 
  
  % color matches (inner)
  y[1,3] = y[3,1] /\ 
  y[2,3] = y[6,1] /\ 
  y[4,3] = y[8,1] /\ 
  y[5,3] = y[11,1] /\ 
  y[7,3] = y[13,1] /\ 
  y[9,3] = y[15,1] /\ 
   
  y[14,2] = y[15,3] /\ 
  y[7,2] = y[8,3] /\ 
  y[12,2] = y[13,3] /\ 
  y[2,2] = y[3,3] /\ 
  y[5,2] = y[6,3] /\ 
  y[10,2] = y[11,3] /\ 
  
  y[15,2] = y[16,1] /\ 
  y[8,2] = y[9,1] /\ 
  y[13,2] = y[14,1] /\ 
  y[3,2] = y[4,1] /\   
  y[6,2] = y[7,1] /\   
  y[11,2] = y[12,1]
;


% color code: 1 - red, 2 - green, 3 - white, 4 - blue, 5 - black, 6 - yellow
% each row describes a triangle in each of three possible orientations

piece_col =  
array3d(P,O,S, [ 
1,2,3,3,1,2,2,3,1,
1,2,5,5,1,2,2,5,1,
1,2,6,6,1,2,2,6,1,
1,3,6,6,1,3,3,6,1,
1,5,2,2,1,5,5,2,1,
1,5,2,2,1,5,5,2,1,
2,4,6,6,2,4,4,6,2,
2,5,5,5,2,5,5,5,2,
2,5,6,6,2,5,5,6,2,
2,6,3,3,2,6,6,3,2,
2,6,3,3,2,6,6,3,2,
3,3,4,4,3,3,3,4,3,
3,4,4,4,3,4,4,4,3,
3,4,5,5,3,4,4,5,3,
3,4,5,5,3,4,4,5,3,
4,6,5,5,4,6,6,5,4
])
;

output [
  if j = 1 then "\n" else " " endif ++
  show(y[i,j])
  | i in P, j in S
] ++ ["\n"];
