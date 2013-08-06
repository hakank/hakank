%
% Locker puzzle in Minizinc
%
%  http://www-lp.doc.ic.ac.uk/UserPages/staff/ft/alp/humour/num
% 
% """
% Appeared in Volume 8/4, November 1995
% 
% 1. My locker combination is made up of three numbers.
% 2. The numbers on the dial range from 1-40.
% 3. The second number is twice the first.
% 4. The third number is ten more than the second.
% 5. The first number has one even digit and one odd digit.
% 6. The second number has either two odd or two even digits.
% 7. The total of all the different digits used in the combination is
%    10.
%    (For example: 22/22/22 would total 2, and 18/28/12 would total 11.)
% 
% First: What is the combination?
% 
% Second: Is there any way to rewrite this puzzle with 6 or less rules,
%         without the answer being obvious?
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

predicate odd(var int: x) =
     x mod 2 = 1
;

predicate even(var int: x) =
     x mod 2 = 0
;


%
% toNum
% 
predicate toNum(array[int] of var int: tal, var int: summa,  int: base) =
  let { int: len = length(tal) }
  in
  summa = sum(i in 1..len) (
    ceil(pow(int2float(base), int2float(len-i))) * tal[i]
  )
  /\ forall(i in 1..len) (tal[i] >= 0)
;


set of int: digits = 1..40;
var digits: d1;
array[1..2] of var 0..9: a_d1;
var digits: d2;
array[1..2] of var 0..9: a_d2;
var digits: d3;
array[1..2] of var 0..9: a_d3;

array[1..6] of var 0..9: a = a_d1 ++ a_d2 ++ a_d3;
var int: all_sum = sum(i in 1..6) ( a[i] ); % sum all digits
array[0..9] of var 0..9: a_digits; % which digits is used in a?
var int: used_digits_sum; 


solve satisfy;

constraint
    toNum(a_d1, d1, 10) /\
    toNum(a_d2, d2, 10) /\
    toNum(a_d3, d3, 10) /\

    % 3. The second number is twice the first.
    d2 = 2 * d1 /\
    % 4. The third number is ten more than the second.
    d3 = 10 + d2  /\
    % 5. The first number has one even digit and one odd digit.
    (
      (odd(a_d1[1]) /\ even(a_d1[2])) \/
      (odd(a_d1[2]) /\ even(a_d1[1]))
    )
    /\
    % 6. The second number has either two odd or two even digits.
    (
      (odd(a_d2[1])  /\  odd(a_d2[2])) \/
      (even(a_d2[1]) /\ even(a_d2[2]))
    ) 
   /\
   % 7. The total of all the different digits used in the combination is 10.
   %    I.e. the sum of the _unique_ numbers.
   % 
   %    Use the gcc to get the numbers used...
   global_cardinality(a, array1d(0..9, set2array(index_set(a_digits))), a_digits)
   /\ % ... and then sum those numbers.
   used_digits_sum = sum(i in 0..9) (
     i * bool2int(a_digits[i] > 0)
   )
   /\ 
   used_digits_sum = 10
;


output
[
  show(a_digits)
];
