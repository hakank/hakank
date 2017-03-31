% 
% n queens problem in MiniZinc.
% 
% Using alldifferent. 
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";

int: n;
array[1..n] of var 1..n: queens;


solve :: int_search(
        queens, 
        first_fail,
        indomain_median,
        complete
       ) 
     satisfy;

% solve satisfy;

% Chuffed (with --free) seems to perform better without :: domain
% on the first constraint.
constraint all_different(queens); % :: domain;
constraint all_different([queens[i]+i | i in 1..n]); % :: domain;
constraint all_different([queens[i]-i | i in 1..n]); % :: domain;

output 
[
  show(queens)
] 
% ++ 
% [
%   if j = 1 then "\n" else "" endif ++
%      if fix(queens[i]) = j then          
%         % show_int(2,j)
%         "Q"
%      else
%         "#"
%      endif
%   | i in 1..n, j in 1..n
% ] ++
% ["\n"]
;

% data
n = 8;
