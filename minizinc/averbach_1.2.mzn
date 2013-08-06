%
% From Averbach & Chein "Problem Solving Through Recreational Mathematics", 
% page 2, problem 1.2
% 
% """
% Ms X, Ms Y, and Ms Z - and American woman, and Englishwoman, and a Frenchwoman, but not
% neccessarily in that order, were seated around a circular table, playing a game of Hearts.
% Each passed three cards to the person on her right.
% Ms Y passed three hearts to the American, 
% Ms X passed the queen of spades and two diamonds to the person who passed her cards
% to the Frenchwoman
% 
% Who was the American? The Englishwoman? The Frenchwoman?
% """"
% This model gives the following solution
% table: [1, 2, 3]
% [American, English, French]: [1, 3, 2]
%
%         1                      American
%                      
%     3      2               English   French
%  

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc



include "globals.mzn";
var int: X;
var int: Y;
var int: Z;

var 1..3: American;
var 1..3: English;
var 1..3: French;

array[1..3] of var 1..3: xtable = [X,Y,Z];

solve satisfy;

% x is right to y
predicate rightTo(var 1..3: x, var 1..3: y) =
    x = y + 1 
    \/
    x = y - 2 % around the corner
;

predicate leftTo(var 1..3: x, var 1..3: y) = 
    rightTo(y,x)
;

constraint
   all_different(xtable) /\
   all_different([American, English, French]) /\

   rightTo(Y, American) /\
   leftTo(X, French) /\

   % symmetry breaking
   X = 1
;

output [
  "table: ", show(xtable),"\n",
  "[American, English, French]: ", show([American, English, French]), "\n",
];
