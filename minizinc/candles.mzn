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
array[1..n] of var 0..20: varsnow;
array[1..n] of var 0..20: varsthen;
var 0..100: now;      % number of candles this year
var 0..100: lastyear; % number of candles last year



solve satisfy;
% solve :: int_search(vars ++ [now, twoyearsago], first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 2..n) (
     (varsnow[i-1] = varsnow[i]+1)
     \/ 
     (varsnow[i-1] = varsnow[i])
   )
   /\
   forall(i in 1..n) (
     varsnow[i] = varsthen[i]+1
   )

   /\ nvalue(6,varsnow)  % 6 consecutive years
   /\ nvalue(6,varsthen) % 6 consecutive years
   /\ lastyear + 2 = now

   /\ now = sum(varsnow)
   /\ lastyear = sum(varsthen)
;

output
[
  "varsnow: ", show(varsnow), "\n",
  "lastyear: ", show(lastyear), "\n",
  "now:", show(now), "\n",

];