% 
% Self referential sentence in MiniZinc.
% 
% E.g. from
%  http://puzzling.stackexchange.com/questions/1901/write-a-true-self-reflective-statement-about-the-digits-from-0-to-9-or-prove-it
%  
%  "This statement contains 1 0's, 7 1's, 3 2's, 2 3's, 1 4's, 1 5's, 1 6's, 2 7's, 1 8's, and 1 9's."
%
%  Note: It happens that the number of occurrences of the digits are between 1..9 
%        so we don't have to worry about converting 10 to 1,0 etc.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: b = 9; % base
int: n = 2*(b+1);

% decision variables
array[1..n] of var 0..b: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % The digits 0..b are placed in positions x[i*2+1].
  % Ensure that it is x[i*2+2] occurrences of the digit i
  forall(i in 0..b) (
    x[i*2+1] = i 
    /\
    count(x,i,x[i*2+2])
  )
;

output 
[
  "x: ", show(x), "\n",
  "This statement contains " 
]
++
[
 show(x[i*2+2]) ++ " " ++ show(x[i*2+1]) ++ "'s, " ++  
      if i == b-1 then "and " else "" endif
  | i in 0..b
];

