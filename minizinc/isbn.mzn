% 
% Some explorations of ISBN13 in MiniZinc.
% 

% See
% http://en.wikipedia.org/wiki/ISBN


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 13;
array[1..n] of var 0..9: isbn;
var 1..9: mult0;
var 1..9: mult1;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

  mult0 = 3 /\ mult1 = 1 /\

   % cp1d(isbn, [9,7,8,0,5,2,1,6,4,4,0,8,2])
   % cp1d(isbn, [9,7,8,1,5,5,8,6,0,8,9,0,0])
   % cp1d(isbn, [9,7,8,9,1,9,8,9,9,7,9,1,6])
   cp1d(isbn, [9,7,8,9,1,9,8,9,9,7,9,1,_])
   /\
   isbn[n] = (10 - sum(i in 1..n-1) (
                     if i mod 2 = 0 then
                       isbn[i] * mult0 % 3
                     else
                       isbn[i] * mult1 % 1
                     endif
                   ) mod 10) mod 10
;


output [
  "isbn: ", show(isbn), "\n",
  "mult0: ", show (mult0), " mult1: ", show(mult1), "\n"
];