% 
% Coins puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles:
% http://www.chlond.demon.co.uk/puzzles/puzzles1.html, puzzle nr. 2
% Description  : Coin puzzle
% Source       : Mathematical Puzzles of Sam Loyd (P111)

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% % http://www.chlond.demon.co.uk/puzzles/sol1s2.html
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

array[1..3] of 1..100: v = [15,16,17];
array[1..3] of var int: x;

int: requ = 121; % i.e. 11 * 11

constraint
        forall(i in 1..3) (x[i] >= 0) 
        /\
        sum(i in 1..3) (v[i]*x[i]) = requ
;

solve minimize sum(i in 1..3) (x[i]);

output
[
  "x: " ++ show(x)
];