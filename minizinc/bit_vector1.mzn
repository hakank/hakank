% 
% Bit vector problem in MiniZinc.
%
% From the presentation by 
% Filip Marić and Predrag Janicˇić
% "URBiVa: Uniform Reduction to Bit-Vector Arithmetic"
% (International Joint Conference on Automated Reasoning 2010)
% """
% Alice picks a number. She then multipies it by two. Then she
% inverts the last 4 bits of the obtained result. What is the number
% that Alice picked, if the obtained result is the same as the initial
% pick?
% """
%
% 
%
% At page 9, the presentation says:
% """
% The only solution is na = 0b0000101 = 5.
% """
%
% But then I must have missed something in this model since it gives
% 8 solutions (slightly edited): 
%
%    5, 21, 37, 53, 69, 85, 101, 117
%
% num : 5  num_x  : [0, 0, 0, 0, 0, 1, 0, 1]
% num2: 10 num2_x : [0, 0, 0, 0, 1, 0, 1, 0]
% num3: 5 num3_x  : [0, 0, 0, 0, 0, 1, 0, 1]
% ----------
% num : 21  num_x : [0, 0, 0, 1, 0, 1, 0, 1]
% num2: 42 num2_x : [0, 0, 1, 0, 1, 0, 1, 0]
% num3: 21 num3_x : [0, 0, 0, 1, 0, 1, 0, 1]
% ----------
% num : 37  num_x : [0, 0, 1, 0, 0, 1, 0, 1]
% num2: 74 num2_x : [0, 1, 0, 0, 1, 0, 1, 0]
% num3: 37 num3_x : [0, 0, 1, 0, 0, 1, 0, 1]
% ----------
% num : 53  num_x : [0, 0, 1, 1, 0, 1, 0, 1]
% num2: 106 num2_x: [0, 1, 1, 0, 1, 0, 1, 0]
% num3: 53 num3_x : [0, 0, 1, 1, 0, 1, 0, 1]
% ----------
% num : 69  num_x : [0, 1, 0, 0, 0, 1, 0, 1]
% num2: 138 num2_x: [1, 0, 0, 0, 1, 0, 1, 0]
% num3: 69 num3_x : [0, 1, 0, 0, 0, 1, 0, 1]
% ----------
% num : 85  num_x : [0, 1, 0, 1, 0, 1, 0, 1]
% num2: 170 num2_x: [1, 0, 1, 0, 1, 0, 1, 0]
% num3: 85 num3_x : [0, 1, 0, 1, 0, 1, 0, 1]
% ----------
% num : 101  num_x: [0, 1, 1, 0, 0, 1, 0, 1]
% num2: 202 num2_x: [1, 1, 0, 0, 1, 0, 1, 0]
% num3: 101 num3_x: [0, 1, 1, 0, 0, 1, 0, 1]
% ----------
% num : 117  num_x: [0, 1, 1, 1, 0, 1, 0, 1]
% num2: 234 num2_x: [1, 1, 1, 0, 1, 0, 1, 0]
% num3: 117 num3_x: [0, 1, 1, 1, 0, 1, 0, 1]


% For bits = 16 the model gives 2048 solutions.
% The last is 32757 
% num : 32757  num_x: [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1]
% num2: 65514 num2_x: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0]
% num3: 32757 num3_x: [0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1]



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: bits = 16;

array[1..bits] of var 0..1: num_x;
var int: num;

array[1..bits] of var 0..1: num2_x;
var int: num2;

array[1..bits] of var 0..1: num3_x;
var int: num3;


predicate toNum(array[int] of var int: a, var int: n,  float: base) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search(num_x ++ num2_x ++ num3_x, smallest, indomain_min, complete) satisfy;

constraint
   toNum(num_x, num, 2.0) 
   /\
   num2 = num*2
   /\
   toNum(num2_x, num2, 2.0)
   /\
   toNum(num3_x, num3, 2.0)
   /\
   forall(i in bits-3..bits) (
      num3_x[i] = 1 <-> num2_x[i] = 0
   )
   /\
   num = num3
;

output [
  "num : " ++ show(num)  ++ " num_x : " ++ show(num_x)  ++ "\n" ++
  "num2: " ++ show(num2) ++ " num2_x: " ++ show(num2_x) ++ "\n" ++
  "num3: " ++ show(num3) ++ " num3_x: " ++ show(num3_x) ++ "\n"
]
 ++ ["\n"]
;

