% 
% Enigma 1575 (All our days) in MiniZinc.
% 
% Problem formulation from Formula1 Compiler:
% http://www.f1compiler.com/samples/Enigma%201575.f1.html
% """
% Enigma 1575 Adrian Somerfield, New Scientist magazine, December 12, 2009.
% 
% I have written the days of the week with their ordinal numbers as shown:
% 
%   MO TU  W TH  F SA SU
%    1  2  3  4  5  6  7
% 
% Letters being consistently replaced by non-zero digits, each day is 
% divisible by its own ordinal number (and by no higher digit). 
% Please send in the FAMOUS number.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 7;
set of int: S = 1..9;
var S: m;
var S: o;
var S: t;
var S: u;
var S: w;
var S: h;
var S: f;
var S: s;
var S: a;
array[1..9] of var 1..9: letters = [m,o,t,u,w,h,f,s,a];

array[1..7] of var 1..99: days;
var 0..699999: famous = f*100000 + a*10000 + m*1000 + o*100 + u*10 + s;

% solve satisfy;
solve :: int_search(letters ++ days ++ [famous], first_fail, indomain_median, complete) satisfy;

constraint
   all_different(letters) /\

   days[1] = 10*m + o /\
   days[2] = 10*t + u /\
   days[3] =        w /\
   days[4] = 10*t + h /\
   days[5] =        f /\
   days[6] = 10*s + a /\
   days[7] = 10*s + u /\

   forall(i in 1..7) (
      % is divisible by its own ordinal number
      days[i] mod i = 0
      /\ % and by no higher digit
      forall(j in i+1..9) (
         days[i] mod j != 0
      )
   )
;


output
[
  "letters: " ++ show(letters) ++ "\n" ++
  "days: " ++ show(days) ++ "\n"
];
