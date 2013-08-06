% 
% Fifty puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 5. 
% Description  : Fifty puzzle
% Source       : The Puzzles of Sam Loyd (P 54)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol1s5.html
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 10;
array[1..n] of 1..30: v = [3, 6, 9, 12, 15, 19, 21, 25, 27, 30];
array[1..n] of var 0..1: x;
var int: sumx = sum(i in 1..n) (x[i]);

% solve minimize sumx;
solve :: int_search(x, first_fail, indomain_min, complete) minimize sumx;

constraint 
   sum(i in 1..n) (v[i]*x[i]) = 50
   % /\ v = [3, 6, 9, 12, 15, 19, 21, 25, 27, 30]
;

output [
       show(v[i]) ++ ": " ++ show(x[i]) ++ "\n"
       | i in 1..n
];

% output [
%   show_cond(x[i] > 0, v[i], "") ++ show_cond(x[i] > 0, " ", "")
%   | i in 1..n
% ];
