% 
% Dimes puzzle in MiniZinc.
%
% http://brainyplanet.com/index.php/Dimes?PHPSESSID=051ae1e2b6df794a5a08fc7b5ecf8028
% From rec.puzzles FAQ
% """
% "Dad wants one-cent, two-cent, three-cent, five-cent, and ten-cent stamps. 
% He said to get four each of two sorts and three each of the others, but I've 
% forgotten which. He gave me exactly enough to buy them; just these dimes." 
% How many stamps of each type does Dad want? A dime is worth ten cents. 
% -- J.A.H. Hunter
% """
% 
% Note: The solution states one solution
% http://brainyplanet.com/index.php/Dimes%20Solution?PHPSESSID=051ae1e2b6df794a5a08fc7b5ecf8028 
% """
% The easy way to solve this is to sell her three each, for 3x(1+2+3+5+10) = 63
% cents. Two more stamps must be bought, and they must make seven cents 
% (since 17 is too much), so the fourth stamps are a two and a five.
% """

% I.e. the solution 
%   amount = [3, 4, 3, 4, 3]
%   amount_gcc = [3, 2]
%   cents = 70

% But there is one other solution:
%   amount = [3, 4, 3, 4, 4]
%   amount_gcc = [2, 3]
%   cents = 80

% 
% Note2: 
% I've let values variable so we can study other things, say the maximum 
% amount with "free" (ordered and distinct) values. There are 476
% different solutions for values between 1..10.
% 


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..5] of var 1..10: values; % = [1,2,3,5,10];
array[1..5] of var 3..4: amount;
array[3..4] of var 0..4: amount_gcc;
var 1..10000: cents; 

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
solve :: int_search(values ++ amount ++ amount_gcc, first_fail, indomain_max, complete) satisfy;
% solve :: int_search(values ++ amount ++ amount_gcc, "first_fail", "indomain_max", "complete") maximize cents;

constraint
  all_different(values)
  /\
  increasing(values)
  /\
  cp1d(values,[1,2,3,5,10])
  /\
  cents = sum(i in 1..5) (values[i]*amount[i])
  /\
  cents mod 10 = 0 % should be even dime
  /\
  % global_cardinality_old(amount, amount_gcc)
  global_cardinality(amount,array1d(3..4, set2array(index_set(amount_gcc))), amount_gcc)
  /\
  (
  cp1d(amount_gcc,array1d(3..4,[2,3]))
  \/
  cp1d(amount_gcc,array1d(3..4,[3,2]))
  )
;

output
[
  "amount: " ++ show(amount) ++ "\n" ++
  "amount_gcc: " ++ show(amount_gcc) ++ "\n" ++
  "cents: " ++ show(cents) ++ "\n"
];