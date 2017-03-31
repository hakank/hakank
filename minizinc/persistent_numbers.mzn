% 
% Persistent numbers (Martin Gardner) in MiniZinc.
% 
% From Martin Gardner (formulation from http://puzzling.stackexchange.com/questions/6507/martin-gardner-persistence )
% """
% A number's persistence is :
% - The number of steps required to reduce it to a single digit by multiplying all 
%   its digits to obtain a second number
%
% - Then multiplying all the digits of that number to obtain a third number, and so on 
%   until a one-digit number is obtained.
%
% For example : 77 has a persistence of four because it requires four steps to reduce 
% it to one digit: 77-49-36-18-8.
% The smallest number of persistence one is 10
% The smallest of persistence two is 25
% The smallest of persistence three is 39
% The smaller of persistence four is 77
%
% What is the smallest number of persistence five?
% """
%
% 
% https://en.wikipedia.org/wiki/Persistence_of_a_number
% """
% In mathematics, the persistence of a number is the number of times one must apply a 
% given operation to an integer before reaching a fixed point at which the operation 
% no longer alters the number.
% 
% Usually, this involves additive or multiplicative persistence of an integer, which is 
% how often one has to replace the number by the sum or product of its digits until one 
% reaches a single digit. Because the numbers are broken down into their digits, the additive 
% or multiplicative persistence depends on the radix. In the remainder of this article, 
% base ten is assumed.
% """

% List of the minimum number for persistence P:
%  P Min number
% -------------
%  1 10
%  2 25
%  3 39
%  4 77
%  5 679
%  6 6788
%  7 68889
%  8 2677889
%  9 26888999
% 10 3778888999
% 11 277777788888899
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_steps = 5; % i.e. the persistence
int: max_size = num_steps;

% decision variables
var 0..pow(10,max_size)-1: num; % the number to start with

array[0..num_steps, 1..max_size] of var 0..9: x;
array[0..num_steps] of var 1..pow(10,max_size)-1: nums;

% functions
function var int: to_num_base(array[int] of var int: a, int: base) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(base, len-i) * a[i] 
                 );
         } in n
;

% base 10
function var int: to_num(array[int] of var int: a) = to_num_base(a, 10);

% solve satisfy;
solve minimize num;
% solve maximize num;
% solve :: int_search(array1d(x) ++ nums, first_fail, indomain_split, complete) minimize num;

constraint
  nums[0] = num /\
  nums[0] = to_num([x[0,i] | i in 1..max_size]) /\
  forall(s in 1..num_steps) (
    % note: don't multiply with leading 0's
    nums[s] = product([if x[s-1,i] > 0 then x[s-1,i] else 1 endif | i in 1..max_size]) 
    /\
    nums[s] = to_num([x[s,i] | i in 1..max_size])
  )
  
  /\ % we want a solution in exactly num_steps steps
  nums[num_steps] != nums[num_steps-1] 

  % redundant constraints
  /\ % requires that last number is a single digit
  forall(i in 1..max_size-1) ( x[num_steps,i] = 0)

  /\ nums[num_steps] < 10
  % /\ decreasing(nums) % much faster
;

output [
  "nums: \(nums)\n"
]
++
[
  if i = 1 then "\n" else " " endif ++
    show(x[s,i])
  | s in 0..num_steps, i in 1..max_size
]
;

