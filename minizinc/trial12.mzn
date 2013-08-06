% 
% The Logical Labyrinth (Smullyan) puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles4.html, puzzle nr. 3
% Description  : The Logical Labyrinth
% Source       : Smullyan, R., (1991), The Lady or The Tiger, Oxford University Press

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol4s3.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: door  = 9;
int: prize = 3; % 1 = Lady, 2 = Tiger, 3 = Empty

array[1..door, 1..prize] of var 0..1: x; % x(i,j) = 1 if door i hides prize j, else 0
array[1..door] of var 0..1: t; %! t(i) = 1 if statement on door i is true, else 0

solve satisfy;

constraint

  % if statement on door 1 is true [i.e. x[1,1]+x[3,1]+x[5,1]+x[7,1]+x[9,1] = 1 ] 
  %                                       then t[1] = 1, else t[1] = 0
  t[1] = x[1,1]+x[3,1]+x[5,1]+x[7,1]+x[9,1] 

  /\ % if statement on door 2 is true [i.e. x[2,3]=1] then t[2] = 1, else t[2] = 0
  t[2] = x[2,3] 

  /\ % if statement on door 3 is true [i.e. t[5]+x[1,1] > 1 ] then t[3] = 1, else t[3] = 0
  t[5]+x[1,1]-2*t[3] <= 0 /\ 
  t[5]+x[1,1]-t[3] >= 0 

  /\ % if statement on door 4 is true [i.e. t[1] = 0] then t[4] = 1, else t[4] = 0
  t[4] = 1-t[1] 

  /\ % if statement on door 5 is true [i.e. t[2]+t[4] > 1] then t[5] = 1, else t[5] = 0
  t[2]+t[4]-2*t[5] <= 0 /\ 
  t[2]+t[4]-t[5] >= 0 /\ 

  % if statement on door 6 is true [i.e. t[3] = 0 ] then t[6] = 1, else t[6] = 0
  t[6] = 1-t[3] /\ 

  % if statement on door 7 is true [i.e. x[1,1] = 0] then t[7] = 1, else t[7] = 0
  t[7] = 1-x[1,1] /\ 

  % if statement on door 8 is true [i.e. x[8,2]+x[9,3] = 2 ] then t[8] = 1, else t[8] = 0
  x[8,2]+x[9,3]-2*t[8] <= 1 /\ 
  x[8,2]+x[9,3]-2*t[8] >= 0 /\ 

  % if statement on door 9 is true [i.e. x[9,2]+t[3] = 2] then t[9] = 1, else t[9] = 0
  x[9,2]+t[3]-2*t[9] <= 1 /\ 
  x[9,2]+t[3]-2*t[9] >= 0 /\ 

  % each door hides 1 prize
  forall(i in 1..door) (
     sum(j in 1..prize) (x[i,j]) = 1 
  )
  /\
  % only one room contains lady
  sum(i in 1..door) (x[i,1]) = 1
  /\
  % sign on lady's door is true
  forall(i in 1..door) (
    t[i] >= x[i,1] 
  )
  /\
  % sign on tigers' doors are false
  forall(i in 1..door) (
     t[i] <= 1 - x[i,2]
  )
  /\

  % if room 8 is empty then not enough information to pinpoint lady
  % min and max x[7,1] give different results
  % room 8 is empty
  % x[8,3] = 1 /\

  % if room 8 is not empty then enough information
  % min and max x[7,1] gives same results
  % if the prisoner was able to deduce where the lady was then
  % room 8 must not have been empty

  % room 8 is not empty
  x[8,3] = 0
;

output [
  if j = 1 then "\n" else " " endif ++
  show(x[i,j])
  | i in 1..door, j in 1..prize
] ++ ["\n"];