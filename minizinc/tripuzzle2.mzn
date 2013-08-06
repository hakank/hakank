%
% Tripuzzle in MiniZinc
%
% Problem and OPL model in
% Martin J Chlond "Tri-Puzzle: A Three-Cornered Conundrum"
% http://ite.pubs.informs.org/Vol6No1/Chlond/index.php
% 
% Note in the original OPL model:
% 
% Model name   : tripuzzle2.mod
% Author       : M J Chlond
% Description  : tri-puzzle
% Source       : Lagoon Games
% Date written : 27/8/05 

int: n;
int: color;

set of 1..n: N = 1..n;
set of 1..n*n: P = 1..n*n;   % pieces and positions
set of 1..3: S = 1..3;     % spots and orientations

array[N] of int: right;
array[N] of int: left;
array[N] of int: base;
array[P,S,S] of int: piece_col;
                
array[P,P,S] of var 0..1: x;
array[P,S] of var int: y;

solve satisfy;

constraint
  forall(i in P, j in S) (
     y[i,j] >= 0
  )
  /\  
  % each position occupied by one triangle in one orientation
  forall(j in P) (
    sum(i in P, k in S) (x[i,j,k]) = 1
  )
  /\
  % each triangle in one position and one orientation
  forall(i in P) (
    sum(j in P, k in S) (x[i,j,k]) = 1
  )
  /\
  % spot colours consistent with piece positions and orientations  
  forall(i in P, j in P, k in S, m in S) (
    y[j,m] >= piece_col[i,k,m] - color*(1-x[i,j,k]) /\
    y[j,m] <= piece_col[i,k,m] + color*(1-x[i,j,k])
  )
  /\
  % color matches (outer)
  forall(i in N) (
    y[(i-1)*(i-1)+1,1] = left[i] /\
    y[i*i,2] = right[i] /\
    y[(n-1)*(n-1)+2*(i-1)+1,3] = base[i] 
  )
  /\
  % color matches (inner)
  forall(i in 1..n-1,j in 1..i) (
    y[-2*i+2*j+i*i,3] = y[2*j+i*i,1] /\
    y[-8*i+6*j+i*i+j*j-2*i*j+16,2] = y[-8*i+6*j+i*i+j*j-2*i*j+17,3] /\
    y[-8*i+6*j+i*i+j*j-2*i*j+17,2] = y[-8*i+6*j+i*i+j*j-2*i*j+18,1]
  )

;
                
n = 4;
color = 6;
left = [2,2,3,2];
right = [6,3,1,3];
base = [5,2,1,4];

% color code: 1 - red, 2 - green, 3 - white, 4 - blue, 5 - black, 6 - yellow
% each row describes a triangle in each of three possible orientations

piece_col =   
array3d(P,S,S,
[1,2,3,3,1,2,2,3,1,
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
