% 
% Magic series in MiniZinc.
% 
% http://mathworld.wolfram.com/MagicSeries.html
% """
% A set of n distinct numbers taken from the interval [1,n^2] form a magic 
% series if their sum is the nth magic constant
%   M_n=1/2n(n^2+1)
%
% (Kraitchik 1942, p. 143). The numbers of magic series of orders 
%   n=1, 2, ..., are 1, 2, 8, 86, 1394, ... (Sloane's A052456). The following 
% table gives the first few magic series of small order.
%  n          magic series
%  1          {1}
%  2          {1,4}, {2,3}
%  3          {1,5,9}, {1,6,8}, {2,4,9}, {2,5,8}, {2,6,7}, {3,4,8}, {3,5,7}, {4,5,6}
%
% """

% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
int: n2 = n*n;
int: m = n*(n2+1);

% decision variables
array[1..n] of var 1..n2: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  sum(x)*2 = m /\
  all_different(x) /\
  increasing(x) 
;

output [ show(x) ];

