% 
% Kakuro puzzle in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Kakuro
% """
% The object of the puzzle is to insert a digit from 1 to 9 inclusive 
% into each white cell such that the sum of the numbers in each entry 
% matches the clue associated with it and that no digit is duplicated in 
% any entry. It is that lack of duplication that makes creating Kakuro 
% puzzles with unique solutions possible, and which means solving a Kakuro 
% puzzle involves investigating combinations more, compared to Sudoku in 
% which the focus is on permutations. There is an unwritten rule for 
% making Kakuro puzzles that each clue must have at least two numbers 
% that add up to it. This is because including one number is mathematically 
% trivial when solving Kakuro puzzles; one can simply disregard the 
% number entirely and subtract it from the clue it indicates.
% """

%
% This model was inspired by the BProlog model kakuro.pl
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";
int: n = 20;
set of int: R = 1..9;

% decision variables
var R: X1;
var R: X2;
var R: X3;
var R: X4;
var R: X5;
var R: X6;
var R: X7;
var R: X8;
var R: X9;
var R: X10;
var R: X11;
var R: X12;
var R: X13;
var R: X14;
var R: X15;
var R: X16;
var R: X17;
var R: X18;
var R: X19;
var R: X20;

array[1..n] of var R: X = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20];

solve satisfy;

predicate word(array[int] of var int: y, var int:s ) =
   alldifferent(y) /\
   s = sum(y)
;

constraint
    word([X1,X2],3) /\
    word([X3,X4],6) /\
    word([X5,X6,X7,X8,X9],18) /\
    word([X10,X11,X12],23) /\
    word([X13,X14],9) /\
    word([X15,X16],6) /\
    word([X17,X18],15) /\
    word([X19,X20],12) /\
    
    word([X1,X5],4) /\
    word([X13,X17],17) /\
    word([X2,X6,X10,X14,X18],22) /\
    word([X7,X11],16) /\
    word([X3,X8,X12,X15,X19],16) /\
    word([X4,X9],3) /\
    word([X16,X20],14)
;

output [ show(X) ];