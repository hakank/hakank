% 
% Before and After puzzle in MiniZinc.
% 
% http://l4f.cecs.anu.edu.au/puzzles/expert/before-and-after
% """
% OK, same four kids, different jellybeans. At the start of this 
% exciting adventure, they each have a different number and all 
% the numbers are greater than 1 and less than 10. David gives some 
% (one or more) of his beans to Alice, and Claire gives some but not all 
% of hers to Boris. Then Boris gives some to Alice (lucky Alice!) and 
% David gives some to Claire. All of the numbers of beans given in these 
% transfers are different, and each is less than 7.
% 
% As a result of all that, they again each have a different number, and 
% again all the numbers are greater than 1 and less than 10. No child has 
% the same number of beans after as any child (themselves included) did before.
% 
% At the end, Alice has one more than David and Claire has one less than Boris.
% Alice gets more from David than she does from Boris.
%
% How many does each of them have, before and after?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: Alice = 1;
int: Boris = 2;
int: Claire = 3;
int: David = 4;

int: n = 4;
int: num_steps = 3;
int: num_gifts = 4;

% decision variables
array[1..num_steps, 1..n] of var 2..9: x;

array[1..num_gifts] of var 1..6: gifts;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   % At the start of this exciting adventure, they each have a different number 
   % and all the numbers are greater than 1 and less than 10. 
   all_different([x[1,i] | i in 1..n]) % step 1

   % David gives some (one or more) of his beans to Alice, and Claire gives 
   % some but not all of hers to Boris. 
   % step 2
   
   % Gift 1: David -> Alice
   /\ x[2,David] = x[1,David] - gifts[1]
   /\ x[2,Alice] = x[1,Alice] + gifts[1]
   /\ gifts[1] <= x[1,David]

   % Gift 2: Claire to Boris
   /\ x[2,Claire] = x[1,Claire] - gifts[2]
   /\ x[2,Boris] = x[1,Boris] + gifts[2]
   /\ gifts[2] < x[1,Claire]

   % Then Boris gives some to Alice (lucky Alice!) and David gives some to Claire. 
   % step 3

   % Gift 3: Boris -> Alice
   /\ x[3,Boris] = x[2,Boris] - gifts[3]
   /\ x[3,Alice] = x[2,Alice] + gifts[3]
   /\ gifts[3] <= x[2,Boris]

   % Gift 4: David -> Claire
   /\ x[3,David] = x[2,David] - gifts[4]
   /\x[3,Claire] = x[2,Claire] + gifts[4]
   /\ gifts[4] <= x[2,David]

   % All of the numbers of beans given in these transfers are different, 
   % and each is less than 7.
   /\ all_different(gifts)

   % As a result of all that, they again each have a different number, and 
   % again all the numbers are greater than 1 and less than 10. 
   % No child has the same number of beans after as any child 
   % (themselves included) did before.
   /\ all_different([x[1,i] | i in 1..n] ++ [x[3,i] | i in 1..n]) 

   % 
   % At the end, Alice has one more than David and Claire has one 
   % less than Boris.  
   %
   /\ x[3,Alice] = x[3,David] + 1
   /\ x[3,Claire] + 1 = x[3,Boris]
  
   % Alice gets more from David than she does from Boris.
   /\ gifts[1] > gifts[3]
;

output 
[
  "gifts: " ++ show(gifts) ++ "\n",
  "A B C D"
]
++
[
  if i = 1 then "\n" else " " endif ++
    show(x[t,i])
  | t in 1..num_steps, i in 1..n
];

