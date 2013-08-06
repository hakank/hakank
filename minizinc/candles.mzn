% 
% Candles problem in MiniZinc.
% 
% From the Choco distribution: samples/Examples/Candles.java
% """
% Marshall and Lily have 7 children borned on Thanksgiving of
% 6 consecutives years.
% Today, uncle Barney, like every year, prepare a birthday cake with candles.
% This year, he has bought two more candles that last year.
% How old are the children and how many candles did Barney buy?
% """
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 7;

% decision variables
array[1..n] of var 1..20: vars;
% var 0..100: now          = sum(vars);  % number of candles this year
var 0..100: now;  % number of candles this year
var 0..100: twoyearsago  =  now - 2*n; % number of candles two years ago

solve satisfy;
% solve :: int_search(vars ++ [now, twoyearsago], first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 2..n) (
      vars[i-1] = vars[i]+1 
   )
   /\
   twoyearsago*2 = now
   /\ 
   now = sum(vars)
;

output
[
  "vars: " ++ show(vars) ++ "\n" ++
  "twoyearsago: " ++ show(twoyearsago) ++ "\n" ++
  "now:" ++ show(now) 
];