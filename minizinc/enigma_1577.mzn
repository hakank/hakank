% 
% Enigma #1577 (Happy New Year) in MiniZinc.
% 
% From New Scientist:
% http://www.newscientist.com/article/mg20527420.900-enigma-number-1577.html
% """
% 06 January 2010 by Richard England
%
% Happy New Year
% 
% I have written down three 3-digit numbers which between them use 
% nine different digits. One of the numbers is a perfect square and 
% another is a triangular number. The sum of the three numbers is 2010.
% 
% What, in ascending order, are the three numbers?
% """
%
% Note: Compare this with the Pandigital Numbers model
%       * http://www.hakank.org/minizinc/pandigital_numbers.mzn
% However, this problem is  easier since we know that there are 
% three 3 digit numbers.
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..9] of var 0..9: x;
var 100..999: num1; % first number
var 100..999: num2; % second number
var 100..999: num3; % third number

% The three digit triangular numbers. 
% From http://www.research.att.com/~njas/sequences/A000217
set of int: triangular = {105, 120, 136, 153, 171, 190, 210, 231, 253, 276, 300, 325, 351, 378, 406, 435, 465, 496, 528, 561, 595, 630, 666, 703, 741, 780, 820, 861, 903, 946, 990};

% The three digit squares
% From http://www.research.att.com/~njas/sequences/A000290
set of int: squares = {100, 121, 144, 169, 196, 225, 256, 289, 324, 361, 400, 441, 484, 529, 576, 625, 676, 729, 784, 841, 900, 961};

%
% Convert array <-> number in base 10
%
predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;


solve satisfy;
% solve :: int_search(
%           x,
%           first_fail,
%           indomain_min,
%           complete
%          ) satisfy;


constraint

   % unique digits
   all_different(x)

   % 
   % First: construct the pandigital numbers 
   % 
   % convert length of num1, num2, and num3 in x to numbers

   /\ 
   toNum([x[i] | i in 1..3], num1) /\
   toNum([x[i] | i in 4..6], num2) /\
   toNum([x[i] | i in 7..9], num3)

   /\ % symmetry breaking: ascending order
   num1 <= num2 /\  num2 <= num3

   /\ % the sum of the three numbers is 2010
   num1 + num2 + num3 = 2010

   /\ % one of the numbers is a perfect square
   (num1 in squares \/ num2 in squares \/ num3 in squares)

   /\ % and another is a triangular number
   (num1 in triangular \/ num2 in triangular \/ num3 in triangular)

%    let {
%       var 0..99: y
%    } in
%    ( y*y = num1 \/ y*y = num2 \/ y*y = num3 )

;

output
[
  show(x) ++ "\n" ++ 
  "num1: " ++ show(num1) ++ " num2: " ++ show(num2) ++ " num3: " ++ show(num3)
];