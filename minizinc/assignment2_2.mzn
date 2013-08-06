%
% Assignment problem in Minizinc.
% 
% Swimming team example.
%
% Same problem as assignment2.mzn (which see), but used the general
% model file assignment_model.mzn.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


include "assignment_model.mzn";

rows = 4;
cols = 10;

% added some columns, the original has 4 columns.
cost =
[|54, 54, 51, 53,   50,60,70,80,90,100, 
 |51, 57, 52, 52,   40,50,60,70,80, 90,
 |50, 53, 54, 56,   40,50,60,80,93, 69,
 |56, 54, 55, 53,   60,80,40,60,50,100
|]
;

output [
   if i = 1 /\ j = 1 then 
     "\nsumma: " ++ show(summax)
   else "" endif ++
   if j = 1 then "\n" else " " endif ++
   show(x[i,j]) 
   | i in 1..rows, j in 1..cols

];
