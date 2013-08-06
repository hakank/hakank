%
% Assignment problem in Minizinc.
% 
% Winston "Operations Research", page 398, swimming team example
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

%
% General predicate for assignment problem:
%  - x: array of decision variables 0/1: 1 if assigned 0 else
%  - cost: cost matrix
%  - total_sum: total sum, either to calculate or as a constraint
%
predicate assignment(array[int, int] of var 0..1: x, 
                     array[int, int] of int: cost, 
                     var int: s
                 ) = 
  forall(i in index_set_1of2(x)) (sum(j in index_set_2of2(x)) (x[i,j]) = 1) /\
  if card(index_set_1of2(x)) = card(index_set_2of2(x)) then
     forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) = 1)
  else  
     forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) <= 1)
  endif
  /\  
  s = sum(i in index_set_1of2(x), j in index_set_2of2(x)) (x[i,j]*cost[i,j])
;

int: rows;
int: cols;
array[1..rows, 1..cols] of var 0..1: x;
array[1..rows, 1..cols] of int: cost;
var int: total_sum;

solve minimize total_sum;
% solve satisfy;


constraint 
           assignment(x, cost, total_sum)
           % /\ total_sum <= 181
;

%
% data
%
% added some columns, the original has 4 columns
%
rows = 4;
cols = 10;

cost =
[|54, 54, 51, 53,   50,60,70,80,90,100, 
 |51, 57, 52, 52,   40,50,60,70,80, 90,
 |50, 53, 54, 56,   40,50,60,80,93, 69,
 |56, 54, 55, 53,   60,80,40,60,50,100
|]
;

output [
   if i = 1 /\ j = 1 then 
     "\ntotal_sum: " ++ show(total_sum)
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in 1..rows, j in 1..cols

];
