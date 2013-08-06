%
% Generalizing the Seseman Convent Problem in Minizinc.
%
% See seseman.mzn for explanation of the 3 x 3 problem.
% (or http://www.hakank.org/seseman/seseman.cgi )
% 
% n is the length of a border of a n x n square
% There is (n-2)^2 "holes" which means that there
% are n^2 - (n-2)^-2 variables to use.
%
% Note: The summation is only of the borders.
%
% The total sum is variable as well. 
%
% E.g.
%
% For n = 3
%  a b c     x[1,1] x[1,2] x[1,3]      i=1, j=1..3
%  d   e     x[2,1] x[2,2] x[2,3]      j=1, i=1..3
%  f g h     x[3,1] x[3,2] x[3,3]      
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 7;
int: border_sum = n*n;
var 1..(n*n)*(n*n): total_sum;

array[1..n,1..n] of var 0..n*n: x;

solve satisfy;
% solve minimize   sum(i in 1..n, j in 1..n) (x[i,j]);
% solve maximize   sum(i in 1..n, j in 1..n) (x[i,j]);

constraint 

   % zero all cells in the middle of the square (the "holes")
   forall(i in 2..n-1, j in 2..n-1 ) (
      x[i,j] = 0
   ) 
   /\ % sum the borders = border_sum
   sum(i in 1..n) (x[i,1]) = border_sum /\
   sum(i in 1..n) (x[i,n]) = border_sum /\
   sum(j in 1..n) (x[1,j]) = border_sum /\
   sum(j in 1..n) (x[n,j]) = border_sum
   /\
   % all borders must be >= 1
   forall(i in 1..n) (
      forall(j in 1..n) (
            (i = 1 \/ j = 1 \/ i = n \/ j = n) -> x[i,j] >= 1
      )
   )
   /\ % total_sum
   sum(i in 1..n, j in 1..n) (x[i,j]) = total_sum

;

output 
% [
%        "\nborder_sum: " ++ show(border_sum) ++ "\n" ++
%        "total_sum: " ++ show(total_sum) ++ "\n"
% ] ++
["["] ++
[
  if i = 1 \/ i = n then
     "f(" ++ show(border_sum) ++ "),"
  else 
     "  -,"
  endif
  | i in 1..n+1
] 
++
[ if j = 1 then "\n" else "," endif ++
   if fix(x[i,j]) < 10 then "   " else "  " endif ++
   show(x[i,j]) ++
   if j = n then 
      if i = 1 \/ i = n then
        ",f(" ++ show(border_sum) ++ "),"
      else 
        ",   -,"
      endif
   else 
      "" 
   endif
 | i in 1..n, j in 1..n
] ++ ["]\n"];
