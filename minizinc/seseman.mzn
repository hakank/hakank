%
% Seseman's Convent Problem in Minizinc
%
%
% This Minizinc program was written by Hakan Kjellerstrand, hakank@bonetmail.com,
% and is commented in the (swedish) blog post
% Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
% http://www.hakank.org/webblogg/archives/001209.html
%
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% 
% For a (swedish) discussion of this problem se
% Sesemans matematiska klosterproblem samt lite Constraint Logic Programming
% http://www.hakank.org/webblogg/archives/001084.html
% and
% Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
% (using Eclipse code)

% 
% n is the length of a border
% There are (n-2)^2 "holes", i.e.
% there are n^2 - (n-2)^2 variables to find out.
%
% The simplest problem, n = 3 (n x n matrix)
% which is represented by the following matrix:
%
%  a b c 
%  d   e 
%  f g h 
% 
% Where the following constraints must hold:
%
%   a + b + c = border_sum
%   a + d + f = border_sum
%   c + e + h = border_sum
%   f + g + h = border_sum
%   a + b + c + d + e + f = total_sum

% For larger matrices just the borders is summed.

int: n = 3;
int: border_sum = n*n;

var 1..(n*n)*(n*n): total_sum; % = 32;
array[1..n,1..n] of var 0..n*n: x; % the n x n solution

solve satisfy;

constraint 
   % experimentation:
   % total_sum = 24 /\

   % 0:s all the middle cells
   forall(i in 2..n-1, j in 2..n-1 ) (
      x[i,j] = 0
   ) 
   /\
   % sum the borders (border_sum)
   sum(i in 1..n) (x[i,1]) = border_sum /\
   sum(i in 1..n) (x[i,n]) = border_sum /\
   sum(j in 1..n) (x[1,j]) = border_sum /\
   sum(j in 1..n) (x[n,j]) = border_sum
   /\
   % all borders must be >= 1 (may be changed to 0 or whatever)
   forall(i in 1..n) (
      forall(j in 1..n) (
            (i = 1 \/ j = 1 \/ i = n \/ j = n) -> x[i,j] >= 1
      )
   )
   /\ % sum the total
   sum(i in 1..n, j in 1..n) (x[i,j]) = total_sum
;

output [
       if i = 1 /\ j = 1 then "\nborder_sum: " ++ show(border_sum) ++ "\n" ++
       "total_sum: " ++ show(total_sum) else "" endif ++
       if j = 1 then "\n" else " " endif ++
       show(x[i,j]) 
       | i in 1..n, j in 1..n
] ++ ["\n"];
