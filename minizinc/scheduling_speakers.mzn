% 
% Scheduling speakers in MiniZinc.
% 
% From Rina Dechter, Constraint Processing, page 72
% Scheduling of 6 speakers in 6 slots.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6; % number of speakers
array[1..n] of set of 1..n: available; % the available slots 
array[1..n] of var 1..n: x; % the alotted speaker slot

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   all_different(x)
   /\
   forall(i in 1..n) (
      x[i] in available[i]
   )
;

% slots available to speak
available = [
               % Reasoning:             
 {3,4,5,6},    % 2) the only one with 6 after speaker F -> 1
 {3,4},        % 5) 3 or 4
 {2,3,4,5},    % 3) only with 5 after F -> 1 and A -> 6
 {2,3,4},      % 4) only with 2 after C -> 5 and F -> 1 
 {3,4},        % 5) 3 or 4
 {1,2,3,4,5,6} % 1) the only with 1
];

output
[
  show(x)
];
