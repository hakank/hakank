% 
% The Riddle of the Pilgrims puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 2.
% Description  : The Riddle of the Pilgrims
% Source       : Dudeney, H.E., (1949), The Canterbury Puzzles, 7th ed., Thomas Nelson and Sons.

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s2.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
set of 1..2: N = 1..2; % solutions
set of 1..2: F = 1..2; % floors
set of 1..3: R = 1..3; % rows
set of 1..3: C = 1..3; % columns
    
array[N,F,R,C] of var int: x;
  
solve satisfy;

constraint
  forall(i in N, j in F, k in R, l in C) (
     x[i,j,k,l] >= 0
  )
  /\
  % difference between solutions is 3 pilgrims
  sum(j in F,k in R,m in C) (x[1,j,k,m]) + 3 = 
                sum(j in F,k in R,m in C) (x[2,j,k,m])
  /\ % twice as many pilgrims on second floor as first floor
  forall(i in N) (
        sum(k in R,m in C) (2*x[i,1,k,m]) = sum(k in R,m in C) (x[i,2,k,m])
  )
  /\ % eleven on first and third rows (i.e. front and back sides]
  forall(i in N,k in R where k != 2) (
        sum(j in F,m in C) (x[i,j,k,m]) = 11
  )
  /\ % eleven on first and third columnss (i.e. left and right sides]
  forall(i in N,m in C where m != 2) (
        sum(j in F,k in R) (x[i,j,k,m]) = 11
  )
  /\ % at least one pilgrim to a room
  forall (i in N,j in F,k in R,m in C where k != 2 \/ m != 2) (
    x[i,j,k,m] >= 1
  )
  /\ % at most three pilgrims to a room
  forall (i in N,j in F,k in R,m in C where k != 2 \/ m != 2) (
    x[i,j,k,m] <= 3
  )
  /\ % no pilgrims allocated to central celsl
  forall(i in N,j in F) (
    x[i,j,2,2] = 0
  )
;

output [
   if m = 1 then "\n" else " " endif ++
   show(x[i,j,k,m])
   | i in N, j in F, k in R, m in C
] ++ ["\n"];

