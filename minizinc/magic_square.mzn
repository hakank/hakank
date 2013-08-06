%
% Magic squares in MiniZinc
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n = 3;

int: total = ( n * (n*n + 1)) div 2;
array[1..n,1..n] of var 1..n*n: magic;


% solve satisfy;
solve :: int_search(
        [magic[i,j] | i in 1..n, j in 1..n], 
        first_fail,
        indomain_min,
        complete) 
    satisfy;


constraint

        all_different([magic[i,j] | i in 1..n, j in 1..n]) :: domain
        /\
        forall(k in 1..n) (
           sum(i in 1..n) (magic[k,i]) = total % :: domain
           /\
           sum(i in 1..n) (magic[i,k]) = total %:: domain
        )
       /\ % diagonal
       sum(i in 1..n) (magic[i,i]) = total  %:: domain
       /\ % diagonal
       sum(i in 1..n) (magic[i,n-i+1]) = total %:: domain
;

% symmetry breaking
% Activating all these constraints we get the
% "standard" way of counting the number of solutions:
%    1, 0, 1, 880, 275305224
% i.e. this sequence: http://oeis.org/A006052
%
% Without the constraints the number of solutions are:
%  N  #solutions
%  -------------
%  1     1
%  2     0
%  3     8
%  4  7040
%  5  many... 
%
% constraint
%    magic[1,1] < magic[1,n]
%    /\ magic[1,n] < magic[n,1]
%    /\ magic[1,1] < magic[n,n]
% ;


output [
  "Total: " ++ show(total) ++ "\n"
] ++
[
    %   show(magic)
  if j = 1 then "\n" else "" endif ++
    if fix(magic[i,j]) < 10 then " " else "" endif ++
    show(magic[i,j]) ++ " "
  | i,j in 1..n
] 
++ 
["\n"];
