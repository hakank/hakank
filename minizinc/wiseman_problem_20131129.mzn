% 
% Problem from Richard Wiseman in MiniZinc.
% 
% http://richardwiseman.wordpress.com/2013/11/29/its-the-friday-puzzle-235/
% """
% I am thinking of a 5-digit number.   If you put '1' at the beginning of the number 
% you get a number that is three times smaller than if you put '1' at the end of the number.  
% What number am I thinking of?
% """

% Here's a much easier model:
%  var 10000..99999: x;
%  constraint 3*(100000+x) = 10*x + 1; 
%  output [ show(x) ];
%  solve satisfy;

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 



int: n = 5;

% decision variables
array[1..n] of var 0..9: number_a;
var 10000..99999: number;

array[1..n+1] of var 0..9: one_before_a;
var 100000..999999: one_before;
array[1..n+1] of var 0..9: one_after_a;
var 100000..999999: one_after;


predicate to_num(array[int] of var int: a, var int: n) =
  let { int: len = length(a) } in
    n = sum(i in 1..len) ( pow(10, len-i) * a[i] )
;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  to_num(number_a, number) /\
  to_num(one_before_a, one_before) /\
  to_num(one_after_a, one_after) /\

  one_before_a[1] = 1 /\
  one_after_a[n+1] = 1 /\
  forall(i in 1..n) (
    one_before_a[i+1] = number_a[i] /\
    one_after_a[i] = number_a[i] 
  )
  /\
  one_before * 3 = one_after
;

output [
  "number: " ++ show(number) ++ "\n" ++
  "one_before: " ++ show(one_before) ++ "\n" ++
  "one_after: " ++ show(one_after) ++ "\n"
];

