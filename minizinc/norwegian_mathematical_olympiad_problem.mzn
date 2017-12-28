% 
% in MiniZinc.
% 
% https://mindyourdecisions.com/blog/2017/12/10/delightfully-fruitful-problem-from-the-norwegian-math-olympiad-the-cost-of-fruit/
% """
% The numbers a, b, and c are positive integers. An apple cost $a, a banana costs $b, and a cherry costs $c. 
% The cost of b apples, b bananas, and a + b cherries is $77. What would be the cost for 
% one apple, two bananas, and one cherry?
% """

% Note: the solution is y=18.

% However, when all decision variables are show (or there is no no output), there are 12 different solutions,
% all with y=18:
%
% a = 10;
% b = 1;
% c = 6;
% y = 18;
% ----------
% a = 6;
% b = 1;
% c = 10;
% y = 18;
% ----------
% a = 5;
% b = 6;
% c = 1;
% y = 18;
% ----------
% a = 6;
% b = 5;
% c = 2;
% y = 18;
% ----------
% a = 7;
% b = 4;
% c = 3;
% y = 18;
% ----------
% a = 8;
% b = 3;
% c = 4;
% y = 18;
% ----------
% a = 9;
% b = 2;
% c = 5;
% y = 18;
% ----------
% a = 1;
% b = 6;
% c = 5;
% y = 18;
% ----------
% a = 2;
% b = 5;
% c = 6;
% y = 18;
% ----------
% a = 3;
% b = 4;
% c = 7;
% y = 18;
% ----------
% a = 4;
% b = 3;
% c = 8;
% y = 18;
% ----------
% a = 5;
% b = 2;
% c = 9;
% y = 18;
% ----------
% ==========


% Cf Mathematica:
% FindInstance[{b*a + b*b + (a + b)*c == 77, y == a + 2*b + c, a > 0, 
%   b > 0, c > 0, y > 0}, {a, b, c, y}, Integers, 100]
%
% {{a -> 1, b -> 6, c -> 5, y -> 18}, {a -> 2, b -> 5, c -> 6, 
%   y -> 18}, {a -> 3, b -> 4, c -> 7, y -> 18}, {a -> 4, b -> 3, 
%   c -> 8, y -> 18}, {a -> 5, b -> 2, c -> 9, y -> 18}, {a -> 5, 
%   b -> 6, c -> 1, y -> 18}, {a -> 6, b -> 1, c -> 10, 
%   y -> 18}, {a -> 6, b -> 5, c -> 2, y -> 18}, {a -> 7, b -> 4, 
%   c -> 3, y -> 18}, {a -> 8, b -> 3, c -> 4, y -> 18}, {a -> 9, 
%   b -> 2, c -> 5, y -> 18}, {a -> 10, b -> 1, c -> 6, y -> 18}}


% Wolfram Alpha
% http://www.wolframalpha.com/input/?i=Solve+%7Bb*a+%2B+b*b+%2B+(a+%2B+b)*c+%3D%3D+77,+y+%3D%3D+a+%2B+2*b+%2B+c,+a+%3E+0,++++b+%3E+0,+c+%3E+0,+y+%3E+0%7D+Integers

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: total = 77;
var 1..total: a;
var 1..total: b;
var 1..total: c;

var 1..total: y;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
    b*a + b*b + (a+b)*c = total 
    /\ y = a + 2*b + c
    /\ y > 0
    /\ a > 0
    /\ b > 0
    /\ c > 0
;

output ["y=\(y)"]; % this only show a single solution: y=18

