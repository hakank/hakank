% 
% Divisible by unit digits in MiniZinc.
% 
% Martin Gardner problem (picture)
% http://3.bp.blogspot.com/-p7UH0kTXF8A/TWwfpmdSsmI/AAAAAAAACkw/s0kN9x4pkOg/s1600/mm.gif
% """
% The number 64 har the property that is divisivle by its units
% digits. How many whole numbers between 10 and 50 has this
% property?
% """
% (Via OscaR.)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 2;

% decision variables
int: ll = 10;
int: uu = 50;

array[ll..uu] of var 0..1: x;
% var set of ll..uu: x; % set based

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in ll..uu) (
       i mod (i mod 10) = 0 <-> x[i] = 1 % array based
       % i mod (i mod 10) = 0 <-> i in x     % set based
   )
;

output 
[
  "sum: " ++ show(sum(x)) ++ "\n" % array based
  % "sum: " ++ show(card(x)) ++ "\n"  % set based
  ++ 
  show(x) ++ "\n"
]
++
[ % array based
  if fix(x[i]) == 1 then show(i) ++ " " else "" endif
  | i in ll..uu
]
;

