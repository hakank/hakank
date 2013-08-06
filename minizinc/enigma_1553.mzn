% 
% Enigma problem 1553 (Squares from squares) in MiniZinc.
% 
% From New Scientist:
% """
% Squares from squares
%
% Enigma 1553 Susan Denham, New Scientist magazine, July 11, 2009
%
%  
% Your task is to place a digit in each of these 16 squares:
%
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%   |  |  |  |  |  
%   +--+--+--+--+
%
% When you have finished, the grid should have the following properties:
%
% * no digit occurs more than once in any row
% * the sum of the four digits in each row is the same
% * the sum of the four digits in each column is the same
% * each row should form a different four-figure perfect square.
%
% Please send in the four perfect squares in increasing order.
%
% """
%
% Compare with the Formula One model:
% http://www.f1compiler.com/samples/Enigma%201553.f1.html
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
set of int: Digits = 0..9;

array[1..n,1..n] of var Digits: x;
var int: row_sum;
var int: col_sum;
array[1..n] of var 1000..9999: row_squares;

predicate square(var int: x) =
   let {
      var 0..ceil(sqrt(int2float(ub(x)))): y
   } in
   y*y = x
;

predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;


% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n], 
        occurrence, 
        indomain_min, 
        complete) 
    satisfy;

constraint

   row_sum > 0 /\ col_sum > 0 /\

   forall(i in 1..n) (
      % no digit occurs more than once in any row
      all_different([x[i,j] | j in 1..n]) /\

      % the sum of the four digits in each row is the same
      sum([x[i,j] | j in 1..n]) = row_sum
   )

   /\ % * the sum of the four digits in each column is the same
   forall(j in 1..n) (
      sum([x[i,j] | i in 1..n]) = col_sum
   )

   /\ % each row should form a different four-figure perfect square.
   forall(i in 1..n) (
      let {
        var 1000..9999: y
      } in
      toNum([x[i,j] | j in 1..n], y) /\
      square(y) /\
      row_squares[i] = y
   )
   /\
   all_different(row_squares)
   /\
   increasing(row_squares)
;

output
[ if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n
];