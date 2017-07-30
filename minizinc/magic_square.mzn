%
% Magic squares in MiniZinc
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n; % = 6;

int: total = ( n * (n*n + 1)) div 2;
% var 0..n*n*n: total;
array[1..n,1..n] of var 1..n*n: magic;


% solve satisfy;
solve :: int_search(
        [magic[i,j] | i in 1..n, j in 1..n], 
        first_fail,
        indomain_random, % indomain_median,
        complete) 
    satisfy;


constraint
  all_different([magic[i,j] | i in 1..n, j in 1..n]) % :: bounds % domain
  /\
  forall(k in 1..n) (
     sum(i in 1..n) (magic[k,i]) = total /\
     sum(i in 1..n) (magic[i,k]) = total
  )
  /\ % diagonal
  sum(i in 1..n) (magic[i,i]) = total
  /\ % diagonal
   sum(i in 1..n) (magic[i,n-i+1]) = total

  /\ total = ( n * (n*n + 1)) div 2

;


output [
  "Total: " ++ show(total) ++ "\n"
] ++
[
  if j = 1 then "\n" else "" endif ++
    show_int(ceil(log(10.0,int2float(ub(total)))),magic[i,j]) ++ " "
  | i,j in 1..n
] 
++ 
["\n"];
