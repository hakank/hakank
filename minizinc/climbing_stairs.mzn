% 
% Climbing stairs problem in MiniZinc.
% 
% http://plus.maths.org/content/its-long-way-top
% """
% Every time I come home I have to climb a flight of stairs. 
% When I'm feeling energetic I sometimes take two steps at a time. 
% This gives me a number of ways to climb the stairs. 
% For example, if there are ten steps, I could climb them taking 
% five leaps of two, giving the pattern
% 
% 2, 2, 2, 2, 2.
% 
% Or I could only use a leap of two at the beginning and the end, giving the pattern
% 
% 2, 1, 1, 1, 1, 1, 1, 2.
% 
% How many ways are there all together of climbing the ten steps?
% """

%
% For m=2
% n    #sols    #sols w/o symmetry breaking
% -----------------------------------------
%  1    1          1
%  2    2          3
%  3    3          7
%  4    5         19
%  5    8         51 
%  6   13        141        
%  7   21        393
%  8   34       1107
%  9   55       3139
% 10   89       8953
%
% With symmetry breaking: it's a familiar sequence...
% 
% W/o symmetry breaking: 1,3,7,19,51,141,393,1107,3139,8953
% http://oeis.org/A002426: Central trinomial coefficients: largest coefficient of (1+x+x^2)^n.

%
% For m=3
%  n  #sols
%  --------
%  1     1
%  2     2
%  3     4
%  4     7
%  5    13
%  6    24
%  7    44
%  8    81
%  9   149
% 10   274
% 
% 1,2,4,7,13,24,44,81,149,274
% http://oeis.org/A000073: Tribonacci numbers: a(n) = a(n-1) + a(n-2) + a(n-3)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; % steps of the stairs
int: m = 2; % max number of steps to take

% decision variables
array[1..n] of var 0..m: x;
var 0..n: z = sum([x[i] > 0 | i in 1..n]);


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  sum(x) = n 
  /\ % symmetry breaking (all 0 last)
  forall(i in 2..n) (
    if x[i-1] = 0 then forall(j in i..n) ( x[i] = 0 ) else true endif
   )
;

output [show(x)];

