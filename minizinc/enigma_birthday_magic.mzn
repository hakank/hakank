% 
% Enigma puzzle 1448  Birthday Magic in MiniZinc.

% Problem statement from 
% http://www.f1compiler.com/samples/Enigma%201448.f1.html
% "Enigma 1448 Gwyn Owen, New Scientist magazine, June 23 2007."

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
array[1..n, 1..n] of var 0..100: x;
var 0..100: age;
var 1..31: day;
var 1..12: month;
var 0..99: year2;
var 1900..2000: year; % full birth year

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   
   cp2d(x, array2d(1..n, 1..n, 
               [
                  _,  _, 19,  _,
                 39,  _,  1,  6,
                  5,  2,  _,  _,
                  _,  _,  _,  2
               ]))

   /\ % is a magic square
   forall(k in 1..n) (
     sum(i in 1..n) (x[k,i]) = age
     /\
     sum(i in 1..n) (x[i,k]) = age
   )
   /\ % diagonals
   sum(i in 1..n) (x[i,i]) = age /\
   sum(i in 1..n) (x[i,n-i+1]) = age

   /\ % calculate the age, 
      % top row is Day, Month, 19, Year2
   day = x[1,1] /\
   month = x[1,2] /\
   year2 = x[1,4] /\
   year = 100*x[1,3] + year2
   /\
   year + age = 2007

;

output [
  "\nday: ", show(day), "\n",
  "month: ", show(month), "\n",
  "year: ", show(year), "\n",
  "age: ", show(age), 
] ++
[
 if j = 1 then "\n" else " " endif ++
   show(x[i,j])
 | i, j in 1..n
] ++ ["\n"];