% 
% The Crowded Chessboard puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html puzzle nr 5
% Description  : The crowded board
% Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond:
% http://www.chlond.demon.co.uk/puzzles/sol4s5.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: size = 8;
int: piece = 4; % 1 - Queen, 2 - Bishop, 3 - Knight, 4 - Rook

set of int: S = 1..size+4;
set of int: P = 1..piece+4;
set of int: R = 3..size+2;  % real part of board
% Note: The Mosel model defines P only for 1..4 but is 1..8 here
array[P] of 0..21: N = [8,14,21,8,0,0,0,0];
array[S,S,P] of var 0..1: x;

% for the output
array[1..size,1..size] of var 0..10: res;
var int: z = sum(i in R,j in R,k in P) (x[i,j,k]);


% solve minimize z;
% Note: This took about 1:48 minutes for ECLiPSe's eplex.
solve :: int_search(
        [x[i,j,k] | i, j in R, k in P], 
        first_fail, 
        indomain_split, 
        complete) 
    minimize z;

% for solve satisfy
% constraint
%   z <= 51
% ;

constraint
     forall(k in P) (
        sum(i in R,j in R) (x[i,j,k]) = N[k]
     )
;
    
constraint 
   forall(i in R,j in R) (
        sum(k in P) (x[i,j,k]) <= 1
   )
;

% No queens attack each other
constraint 
     forall(i in R) (
        sum(j in R) (x[i,j,1]) <= 1
     )
;

constraint  
   forall(j in R) (
        sum(i in R) (x[i,j,1]) <= 1     
   )
;

constraint forall (i in 2..size+3) (
           sum(k in 1..i) (x[k,i-k+1,1]) <= 1
        )
;

constraint forall(j in 1..size+3) (
        sum(k in j..size+4) (x[k,size+4-k+j,1]) <= 1  
        )
;

constraint forall(j in 1..size+3) (
          sum(k in 1..size-j+5) (x[k,j+k-1,1]) <= 1
        )
;

constraint forall(i in 2..size+3) (
        sum(k in i..size+4) (x[k,k-i+1,1]) <= 1
        )
;

% No bishops attack each other
constraint 
   forall(i in 2..size+3) (
        sum(k in 1..i) (x[k,i-k+1,2]) <= 1
   )
   /\
   forall(j in 1..size+3) (
        sum(k in j..size+4) (x[k,size+4-k+j,2]) <= 1
   ) 
   /\  
    forall(j in 1..size+3) (
        sum(k in 1..size-j+5) (x[k,j+k-1,2]) <= 1
    )
   /\
   forall(i in 2..size+3) (
        sum(k in i..size+4) (x[k,k-i+1,2]) <= 1
   )
;

  % No rooks attack each other
constraint forall(i in R) (
        sum(j in 3..size+2) (x[i,j,4]) <= 1
       )
      /\  
     forall(j in R) (
        sum(i in 3..size+2) (x[i,j,4]) <= 1
     )
;

%  a(i,j,3] = 0 if square {i,i} attacked by knight 
constraint 
    forall(i in R,j in R) (
        x[i-2,j-1,3] + x[i-1,j-2,3] + x[i+1,j-2,3] + x[i+2,j-1,3] + 
        x[i+2,j+1,3] + x[i+1,j+2,3] + x[i-1,j+2,3] + x[i-2,i+1,3] + 99*x[i,j,3] <= 99
      )
;


% Dummy squares not occupied 
constraint 
   sum(i in 1..size+4,j in 1..size+4,k in 1..4 where i < 3 \/ i > size+2 \/ j < 3 \/ j > size+2) (x[i,j,k]) =  0
;

% calculate res (which is the output)
constraint 
   forall(i, j in 1..size) (
      res[i,j] = sum(k in P) (k*x[i+2,j+2,k])
   )

;

output 
["z: ", show(z)]
++
[
  if j = 1 then "\n" else " " endif ++
  show(res[i,j])
  | i,j in 1..size
] ++ ["\n"]
;
