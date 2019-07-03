%
% Assigment problem in Minizinc,
% 
% Winston "Operations Research", page 399
% 
% """
% Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
% are marooned on a desert island with Jennifer Anniston,
% Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
% The 'compatibility matrix' in Table 52 indicate how much happiness
% each couple would experience if the spend all their time toghether.
% The happiness earned by a couple is proportional to the fraction 
% of time the spend toghether. 
% ...
% The optimal solution requires that that each person send all their
% time with one person of the opposite sex, so this result is often
% referred to as the Marriage Theorem.
% """
%  
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

int: rows; 
int: cols;
array[1..rows, 1..cols] of var 0..1: x;
array[1..rows, 1..cols] of int: cost;
array[1..rows] of 1..rows: male;
array[1..cols] of 1..cols: female;

var int: total_cost = sum(i in 1..rows, j in 1..cols) (x[i,j]*cost[i,j]);

solve maximize  total_cost;
% solve satisfy;

constraint 
 forall(i in 1..rows) (
        sum(j in 1..cols) (x[i,j]) = 1
  )
  /\
 forall(j in 1..cols) (
        sum(i in 1..rows) (x[i,j]) = 1
 )
%  /\ total_cost = 30 % for solve satisfy
;

rows= 4;
cols= 4;

male = [1,2,3,4];
% 1 "Tom Cruise"
% 2 "Freddie Prinz Jr"
% 3 "Harrison Ford"
% 4 "Mark LeBlanc"
%;

female = [1,2,3,4];
% 1 "Jennifer Anniston"
% 2 "Courtney Cox"
% 3 "Gwynneth Paltrow"
% 4 "Julia Roberts"
%;

cost= 
[|7, 5, 8, 2,
 |7, 8, 9, 4,
 |3, 5, 7, 9,
 |5, 5, 6, 7
|]
;


output [
   if i = 1 /\ j = 1 then 
     "\ntotal_cost: " ++ show(total_cost) else "" endif 
   ++
   if j = 1 then "\n" else " " endif ++ 
   show(x[i,j])

   | i in 1..rows, j in 1..cols
] ++ ["\n"];

