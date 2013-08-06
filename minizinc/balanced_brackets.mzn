% 
% Generate balanced brackets in MiniZinc.
% 
% This model generates balanced brackets of size m*2.
%
% The number of generated solutions for m:
%
%  m        #
%  ----------
%   1       1
%   2       2
%   3       5
%   4      14
%   5      42
%   6     132
%   7     429
%   8    1430
%   9    4862
%  10   16796
%  11   58786
%  12  208012
%  13  742900
%
% 
% Which - of course - is the Catalan numbers.
%
% http://oeis.org/search?q=1%2C2%2C5%2C14%2C42%2C132%2C429%2C1430%2C4862%2C16796%2C58786%2C208012&language=english&go=Search
% http://oeis.org/A000108


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: m = 13;
int: n = m*2;

array[0..1] of string: s = array1d(0..1, ["[","]"]);

% For cumulative (c):
% +1 if x[i] = "["
% -1 if x[i] = "]"
array[0..1] of int: t = array1d(0..1, [1, -1]);  

% 0: "[", 1: "]"
array[1..n] of var 0..1: x;
array[1..n] of var 0..n: c; % counter (cumulative)

% solve satisfy;
solve :: int_search(
        x ++ c, 
        first_fail, 
        indomain_split, 
        complete) 
    satisfy;


constraint
  x[1] = 0 /\ c[1] = 1  % start

  /\ % cumulative
  forall(i in 2..n) (
     c[i] = c[i-1] + t[x[i]]
  )
  /\ x[n] = 1 
  /\ c[n] = 0 % end

  % Redundant constraint: This might make it faster (but it don't)
  % /\ sum(x) = m
;

output 
[
  "m: " ++ show(m) ++ " n: " ++ show(n) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "c: " ++ show(c) ++ "\n"
]
++
[
  show(s[fix(x[i])]) 
  | i in 1..n
]
++ ["\n"]
;

