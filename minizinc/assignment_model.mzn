%
% Assignment problem in MiniZinc.
%
% General model of assigments. Used by assignment2.mzn .
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc


predicate assignment(array[int, int] of var 0..1: x, 
                     array[int, int] of int: cost, 
                     var int: summa
                 ) = 
   forall(i in index_set_1of2(x)) (sum(j in index_set_2of2(x)) (x[i,j]) = 1) /\
   if card(index_set_1of2(x)) = card(index_set_2of2(x)) then
       forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) = 1)
   else  
       forall(j in index_set_2of2(x)) (sum(i in index_set_1of2(x)) (x[i,j]) <= 1)
   endif
   /\  
   summa = sum(i in index_set_1of2(x), j in index_set_2of2(x)) (x[i,j]*cost[i,j])
;

int: rows;
int: cols;
array[1..rows, 1..cols] of var 0..1: x;
array[1..rows, 1..cols] of int: cost;
var int: summax;

% Minimize the cost
solve :: int_search([x[i,j] | i in 1..rows, j in 1..cols], first_fail, indomain_min, complete)  minimize summax;
% solve satisfy;


constraint 
           assignment(x, cost, summax)
;

