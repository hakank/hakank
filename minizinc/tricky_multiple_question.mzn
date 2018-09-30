% 
% Tricky Multiple Choice Question in MiniZinc.
% 
% http://math.stackexchange.com/questions/2217248/which-answer-in-this-list-is-the-correct-answer-to-this-question
% """
% Which answer in this list is the correct answer to this question?
%
% 1. All of the below.
%
% 2. None of the below.
%
% 3. All of the above.
%
% 4. One of the above.
%
% 5. None of the above.
%
% 6. None of the above.
%
% """

% Via 
% MindYourDecisions: "Can You Solve This Tricky Multiple Choice Question?"
% https://www.youtube.com/watch?v=h_fLW4xlOyA

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;

% decision variables
array[1..n] of var bool: x;


solve satisfy;

constraint
  % 1. All of the below.
  (x[1] <-> x[2] /\ x[3] /\ x[4] /\ x[5] /\ x[6])
  % (x[1] <-> forall([x[i] | i in 2..6]))
  /\
  % 2. None of the below.
  (x[2] <-> not(x[3] \/ x[4] \/ x[5] \/ x[6]))
    /\
  % 3. All of the above.
  (x[3] <-> x[1] /\ x[2])
  /\
  % 4. One of the above.
  (x[4] <-> x[1] \/ x[2] \/ x[3])
  /\
  % 5. None of the above.
  (x[5] <-> not(x[1] \/ x[2] \/ x[3] \/ x[4]))
  /\
  % 6. None of the above.
  (x[6] <-> not(x[1] \/ x[2] \/ x[3] \/ x[4] \/ x[5]))
;

output [
  show(x)
]
++
[
  if fix(x[i]) = true then "\nAnswer \(i) is true" else "" endif

  | i in 1..n
] 
;

