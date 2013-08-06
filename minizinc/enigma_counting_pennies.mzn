% 
% Enigma problem (counting pennies)in MiniZinc.
% 
% Problem formulation from 
% http://www.vbforums.com/showthread.php?p=2108230
% """
% Joe asked Penny to think of four different postive digits and add up the 
% 24 different 4-digit numbers that could be made using the four digits. Then 
% he asked her to subtract just one of the 4-digits numbers from the total and 
% write down the new total. When asked what number she had written down, Penny 
% replied: "122**0".
% 
% Joe didnt quite hear the 4th or 5th digits. But he was able to work out the 
% number Penny had subtracted. What was that number?
% """
% 
% 
% Note: 
% From the comments in the thread it seems to that it is a Enigma problem from
% New Scientist of 23 July 2005. I don't know what Enigma number it is, though.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 4;
int: m = 24;
array[1..m,1..n] of 0..9: perms; % The 24 permutations of 4 items (as a template)

array[1..n] of var 0..9: digits; % the four digits chosen
array[1..m] of var 1000..9999: x;                    % the 24 numbers of the four digits
var 100000..999999: digits_sum;             % sum of the 24 numbers 
array[1..6] of var 0..9: digits_sum_a;        % array representation of the sum

predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) } in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
solve :: int_search(x ++ digits ++ digits_sum_a ++ [digits_sum], first_fail, indomain_min, complete) satisfy;

constraint
  all_different(digits) /\

  digits[1] > 0 /\

  % get all the 24 permutations of the 4 digits
  forall(i in 1..m) (
    toNum([digits[perms[i,j]] | j in 1..n], x[i])
  )
  /\ % the sought sum is from subtracting one of the 24 numbers from the sum of x
  exists(i in 1..m) (
    digits_sum = sum(x) - x[i]
  ) 
  /\ % convert the sum to array and use it as a template
  toNum(digits_sum_a, digits_sum) /\
  cp1d(digits_sum_a, [1,2,2,_,_,0])

  /\ % symmetry breaking 
  increasing(digits)
;


% The 24 different permutations of a 4-digit number.
% It is much easier to hard code these.
perms = array2d(1..m,1..n,
[
1,2,3,4,
1,2,4,3,
1,3,2,4,
1,3,4,2,
1,4,3,2,
1,4,2,3,
2,1,3,4,
2,1,4,3,
2,3,1,4,
2,3,4,1,
2,4,3,1,
2,4,1,3,
3,2,1,4,
3,2,4,1,
3,1,2,4,
3,1,4,2,
3,4,1,2,
3,4,2,1,
4,2,3,1,
4,2,1,3,
4,3,2,1,
4,3,1,2,
4,1,3,2,
4,1,2,3
]);

output
[
 "digits: " ++ show(digits) ++ "\n" ++
 "x: " ++ show(x) ++ "\n" ++
 "digits_sum: " ++ show(digits_sum) ++ "\n" ++
 "digits_sum_a: " ++ show(digits_sum_a) ++ "\n" 
];
