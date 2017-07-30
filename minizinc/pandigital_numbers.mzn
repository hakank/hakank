% 
% Pandigital numbers (any base <= 10) in MiniZinc.
%
% From 
% Albert H. Beiler "Recreations in the Theory of Numbers", quoted from
% http://www.worldofnumbers.com/ninedig1.htm
% """
% [ Chapter VIII : Digits - and the magic of 9 ]
% [ I found the same exposé in Shakuntala Devi's book 
%   "Figuring : The Joy of Numbers" ]
%
% The following curious table shows how to arrange the 9 digits so that 
% the product of 2 groups is equal to a number represented by the 
% remaining digits."
%
%   12 x 483 = 5796 
%   42 x 138 = 5796 
%   18 x 297 = 5346 
%   27 x 198 = 5346 
%   39 x 186 = 7254 
%   48 x 159 = 7632 
%   28 x 157 = 4396 
%   4 x 1738 = 6952 
%   4 x 1963 = 7852
% """
%
% See also
% 
% * MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
% """
% A number is said to be pandigital if it contains each of the digits 
% from 0 to 9 (and whose leading digit must be nonzero). However, 
% "zeroless" pandigital quantities contain the digits 1 through 9. 
% Sometimes exclusivity is also required so that each digit is 
% restricted to appear exactly once.
% """
% 
% * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number
%
%
% Also see my Gecode/R model: http://www.hakank.org/gecode_r/pandigital_numbers.rb

% Notes: 
%  * each digits is constrained to be used exactly once.
%  * Larger bases (>10) is not correct handled by the solvers due to
%    integer overflow.
% 
% The model consists of the following variables:
%  * an array, x, of length base-1 (for integers 1..base-1) or
%    length base (for integer 0..base-1)
%  * num1: first number
%  * num2: second number
%  * len1: length of num1, starts in position x[1]
%  * len2: length of num2, starts in position x[len1+1]
%  * res: the result, starts in position x[len1 + len2 + 1]
%  * vop: the operation: 1 for "*" or 2 for "+"
% 
%  Parameters:
%  * base: which base to use
%  * start: which integer to start with, the range is start..base-1
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: base = 10;      % the base to use
int: max_d = base-1; % max digit
int: start = 1;      % Change to 0 for including 0.
int: x_len = max_d+1-start; % length of the array

array[1..x_len] of var start..max_d: x;
var int: num1; % first number
var int: num2; % second number
var int: res;  % the result
var 1..x_len: len1; % length of first number
var 1..x_len: len2; % length of second number
var 1..2: vop; % the operator: 1 = "*", 2 = "+"

array[1..2] of string: vop_s = ["*","+"];

%
% Convert array <-> number in some base
%
predicate toNum(array[int] of var int: a, var int: n,  int: base) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(int2float(base), int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0 /\ a[i] < base)
;

% solve satisfy;
solve :: int_search(
       x ++ [num1, num2, res, len1, len2, vop],
       first_fail,
       indomain_max,
       complete
      ) satisfy;

constraint

  % unique digits
  all_different(x)


  % convert length of num1 and num2 in x to numbers
  % Note: we must use 'exists' since ranges (..) cannot 
  %       take vars, just pars

  /\ % num1. 
  exists(j in 1..x_len) (
     j = len1 /\
     toNum([x[i] | i in 1..j], num1, base)
  )

  /\  % num2
  exists(j, k in 1..x_len) (
     j = len1+1 /\ 
     k = len1+len2 /\ k > j  /\
     toNum([x[i] | i in j..k], num2, base)
  )

  /\ % the product
  exists(k in 1..x_len) (
     k = len1+len2+1 /\
     toNum([x[i] | i in k..x_len], res, base)
  )

  /\ % the operation (multiplication or addition)
  (
   (num1 * num2 = res /\ vop = 1) 
   % \/
   % (num1 + num2 = res /\ vop = 2) 
  )

  /\ % sanitizing and symmetry breaking
  num1 > 0 /\ num1 < num2
  /\
  num2 > 0 /\ num2 < res
  /\
  res > 0
  /\
  res < 99999

  /\ % if we use 0 (start = 0) then the leading digit of
     % the number must be > 0
  x[1] > 0 
  /\
  x[len1+1] > 0
  /\
  x[len1+len2+1] > 0
;


output 
[
 "x: ", show(x), "\n",
 show(num1), " ", vop_s[fix(vop)], " ", show(num2), " = ", show(res), " (base ", show(base), ")", "\n",
];


% output [
%    "\nx: ", show(x), "\n",
%    "decimal: ", 
%    show(num1), " <op", show(vop), "> ", show(num2), " = ", show(res),
%    "\nstart: ", show(start),
%    % "\nlen1: ", show(len1), " len2: ", show(len2), "\n",
% ] ++
% [ 
%   "\nbase ", show(base), ": ",
% ] ++
% [
%   % as of writing (2009-01-09), this only works with ECLiPSe's solver ic
%   % (MiniZinc/flatzinc-solver have problems with integer overflow)
%   show(x[i]) ++
%   show_cond(i = len1, " <op>",  " ") ++
%   show_cond(i = len1+len2, " = ", " ")
%   | i in 1..max_d
% ] ++
% [
%   "\n",
% ] 
% ;

