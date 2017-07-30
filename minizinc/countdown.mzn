% 
% Countdown problem in MiniZinc.
% 
% From Ruby Quiz #7
% http://www.rubyquiz.com/quiz7.html
% """
% One of the longest-running quiz shows on UK TV is called Countdown, 
% and has a "numbers round". There are some cards laid face down in 
% front of the host - the top row contains 'large' numbers (from the 
% set 25, 50, 75, 100), and the rest are 'small' (1 to 10). Six cards 
% are picked and displayed: the choice is made by one of the contestants, 
% who typically will ask for one large number and five small ones.
% 
% Next, a machine called "Cecil" picks a target number between 100 and 
% 999 at random. The contestants then have 30 seconds to find a way of 
% combining the source numbers using the normal arithmetic operators 
% (+, -, *, /) to make the target number, or to get as close as possible.
% 
% Each source card can be used no more than once. The same applies to any 
% intermediate results, although of course you don't have to explicitly 
% show the intermediate results.
% 
% Example: source 100 5 5 2 6 8, target 522
% 
% 100 * 5 = 500
% 5 + 6 = 11
% 11 * 2 = 22
% 500 + 22 = 522
% 
% or more succinctly, (5*100)+((5+6)*2) = 522
% 
% Another solution is (100+6)*5-8 = 522
% 
% Normal arithmetic rules apply, and in particular you are not allowed to 
% use integer rounding. That is, 7 divided by 3 is 2 1/3, not 2.
% 
% The quiz is to write a program which will accept one target number and 
% a list of source numbers, and generate a solution which calculates 
% the target or a number as close to the target as possible.
% """

% Note: This model is more restricted in how the operators works: It only solve
% solutions of this form:
%
%  Numbers =  [3,5,9,9,10,100]
%  Target  = 564
%  Solution:
%    (((((100/5)*10)-9)*3)-9) = 564
%   100/5 = 20
%   20*10 = 200
%   200-9 = 191
%   191*3 = 573
%   573-9 = 564


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n;
array[1..n] of int: numbers;

% decision variables
array[1..n] of var 1..100: x; % the result: numbers in the final positions
array[1..n] of var 1..n: perm; % permutation of numbers for index in x
array[1..n] of var 1..10000: s; % array of the consecutive results
var int: total; % the total

int: max_ops = 4; % number of operations
% int: max_ops = 6;
array[1..n-1] of var 0..max_ops: ops; % the operations

% array[1..max_ops] of string: ops_string = ["+","-","*","/","/","^"];
array[1..max_ops] of string: ops_string = ["+","-","*","/"];

% solve satisfy;
solve :: int_search(ops ++ x ++ s ++ perm, first_fail, indomain_min, complete) satisfy;

%  res = a op b
predicate make_op(var int: a, var int: b, var int: xop, var int: res) =
  (xop = 1 <-> res = a + b)
  /\
  (xop = 2 <-> res = a - b)
  /\
  (xop = 3 <-> res = a * b)
  /\
  (xop = 4 <-> a = res * b) % division 1
  % /\ 
  % (xop = 5 <-> b = res * a) % division 2
  % /\
  % q(xop = 6 <-> res = a * a) % square
;


%
% make the equationx
%
predicate make(array[int] of var int: y, array[int] of var 1..max_ops: xop, var int: res, array[int] of var int: s) =
   let {
     int: len = length(y),
    }
   in
   s[1]=y[1] /\
   forall(i in 1..len-1) (
      make_op(s[i], y[i+1], xop[i], s[i+1])
   )
   /\
   res = s[len]

;

constraint

   % % cf http://en.wikibooks.org/wiki/Puzzles/Arithmetical_puzzles/Four_4s_Equal...
   % numbers = [4,4,4,4]
   % /\

   % perm is a permutation of position in numbers
   % for the position of x
   all_different(perm)
   /\ % get a permutation of numbers
   forall(i in 1..n) (
     x[i] = numbers[perm[i]]
   )
   /\
   make(x, ops, total,s) 
   %/\ total >= 522 
   %/\ total < 600
   % /\ total = 666
   /\ total = 564
;


%
%
%
n = 6;
% numbers = [100, 5, 5, 2, 6, 8]; % same number twice
% numbers = [100, 5, 3, 2, 6, 8];
numbers = [3,5,9,9,10,100];
% n = 4;
% numbers = [4,4,4,4];

output
[
  "x      : " ++ show(x) ++ "\n" ++
  % "perm   : " ++ show(perm) ++ "\n" ++
  "s      : " ++ show(s) ++ "\n" ++
  % "numbers: " ++ show(numbers) ++ "\n" ++
  "total  : " ++ show(total) ++ "\n" ++
  "ops    : " ++ show(ops) ++ "\n" ++ 
  "ops_str: " ++ show([ops_string[fix(ops[i])] | i in 1..n-1]) ++ "\n"
]
++
[ "(" | i in 1..n-1 ]
++
[
  show(x[i]) ++ if i > 1 then ")" else "" endif 
  ++ show(ops_string[fix(ops[i])])
  | i in 1..n-1
] ++ 
[show(x[n]), ")\n"]
++
[ 
  show(s[i]) ++ show(ops_string[fix(ops[i])]) ++ show(x[i+1]) ++ " = " ++ show(s[i+1]) ++ "\n"
  | i in 1..n-1
]
;