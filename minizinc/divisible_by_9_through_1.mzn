% 
% Divisible by 9 through 1 problem in MiniZinc.
%
% From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
% " Solving Combinatory Problems with LINQ"
% """
% Find a number consisting of 9 digits in which each of the digits 
% from 1 to 9 appears only once. This number must also satisfy these 
% divisibility requirements:
% 
%  1. The number should be divisible by 9.
%  2. If the rightmost digit is removed, the remaining number should 
%     be divisible by 8.
%  3. If the rightmost digit of the new number is removed, the remaining 
%     number should be divisible by 7.
%  4. And so on, until there's only one digit (which will necessarily 
%     be divisible by 1).
% """
% 
% Also, see
% "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
% http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/
%
%
% Compare with the more general ECLiPSe model:
%   http://www.hakank.org/eclipse/divisible_by_9_trough_1.ecl
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

%
% This is a slighly more general model by supporting
% different bases.
% However, using base > 10 giver integer overflow.
% 
% For base <= 10 there are solution (i.e. the array x) 
% for the following
%   2: [1]
%   4: [1, 2, 3] 
%      [3, 2, 1]
%   6: [1, 4, 3, 2, 5] 
%      [5, 4, 3, 2, 1]
%   8: [3, 2, 5, 4, 1, 6, 7]
%      [5, 2, 3, 4, 7, 6, 1]
%      [5, 6, 7, 4, 3, 2, 1]
%  10: [3, 8, 1, 6, 5, 4, 7, 2, 9]

% The ECLiPSe model also finds the following solution 
% in base 14
%  14: [9, 12, 3, 10, 5, 4, 7, 6, 11, 8, 1, 2, 13]
%

int: base = 10;
int: n = base-1; % 9
int: m = ceil(pow(int2float(base),(int2float(n))))-1;% 999999999;
array[1..n] of var 1..n : x; % the digits
array[1..n] of var 0..m : t;      % the numbers. t[1] contains the answer

predicate toNum(array[int] of var int: a, var int: n, int: base) =
  let { int: len = length(a) } in
  n = sum(i in 1..len) (
     ceil(pow(int2float(base), int2float(len-i))) * a[i]
  )
  /\ forall(i in 1..len) (a[i] >= 0)
;

% These three predicates (int_div_mod, int_div, and int_mod) are
% from http://www.g12.cs.mu.oz.au/mzn/div_mod/div_mod.mzn
% And can be used even by the solvers that don't support `mod`.
predicate int_div_mod(var int: x, var int:y, var int:d, var int:r) =
	y != 0 /\
	x == y * d + r /\
	x * r >= 0 /\
	-abs(y) < r /\ r < abs(y) /\
	min(lb(x),-ub(x)) <= d /\ d <= max(ub(x),-lb(x)) /\
	%% the next 5 lines are probably unnecessary for propagation solvers
	if lb(x) > 0 then
	   0 <= r /\ r < max(ub(y),-lb(y)) 
	else if ub(x) < 0 then
	   min(lb(y),-ub(y)) < r /\ r <= 0 
	else true endif endif /\
	if lb(y) > 0 then
	   lb(x) div lb(y) <= d /\ d <= ub(x) div lb(y)
        else if ub(y) < 0  then
           ub(x) div ub(y) <= d /\ d <= lb(x) div ub(y)
	else true endif endif;
        

predicate int_div(var int:x, var int:y, var int:d) =
	let { var min(lb(y),-ub(y))+1 .. max(ub(y),-lb(y))-1: r } in
	int_div_mod(x,y,d,r);

predicate int_mod(var int:x, var int:y, var int:r) =
	let { var min(lb(x),-ub(x)) .. max(ub(x),-lb(x)): d } in
	int_div_mod(x,y,d,r);


% solve satisfy;
% solve :: int_search(t ++ x, occurrence, indomain_max, complete) satisfy;
solve :: int_search(x, occurrence, indomain_max, complete) satisfy;

constraint
   all_different(x) /\
   forall(i in 1..n) (
       toNum([x[j] | j in 1..base-i], t[i], base) /\
       % t[base-i] mod i = 0
       int_mod(t[base-i], i, 0)
   )    
;


output [
  "x: " ++ show(x) ++ "\n" ++
  "t: " ++ show(t) ++ "\n"
] 
++ ["\n"];