% 
% Joe's Pyramid (Enigma 1631) in MiniZinc.
%
% http://www.newscientist.com/article/mg20927971.100-enigma-number-1631.html#.VHDqdIXgjR4
% """
% Enigma 1631 - Joe's pyramid
% New Scientist magazine, 29 January 2011.
% By Bob Walker.
% 
% Joe's Pyramid
% 
% This is Joe's pyramid. Every stone is marked with a different one or 
% two digit positive number.
% [
%          [X]
%         [ ][ ]
%       [ ][ ][ ]
%     [ ][ ][ ][ ]
%    [ ][ ][ ][ ][ ]
%  [ ][ ][ ][ ][ ][ ]
%
% represented as
%          1
%        2  3
%      4  5  6
%    7  8  9  10
%  11 12 13 14 15
%16 17 18  19  20  21
%
% ]
% 
% Where a stone rests on two others, its number is the sum of the numbers marked on the 
% two stones on which it rests.
% What number is X?
% """
% 
% There is only two solutions, both have X = 98
% 98, 46, 52, 26, 20, 32, 17, 9, 11, 21, 12, 5, 4, 7, 14, 10, 2, 3, 1, 6, 8]
% ----------
% [98, 52, 46, 32, 20, 26, 21, 11, 9, 17, 14, 7, 4, 5, 12, 8, 6, 1, 3, 2, 10]
% ----------
% ==========


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 21;

% decision variables
array[1..n] of var 0..99: x;



solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
 all_different(x) /\

x[1] = x[2] + x[3] /\
x[2] = x[4] + x[5] /\
x[3] = x[5] + x[6] /\
x[4] = x[7] + x[8] /\
x[5] = x[8] + x[9] /\
x[6] = x[9] + x[10] /\
x[7] = x[11] + x[12] /\
x[8] = x[12] + x[13] /\
x[9] = x[13] + x[14] /\
x[10] = x[14] + x[15] /\
x[11] = x[16] + x[17] /\
x[12] = x[17] + x[18] /\
x[13] = x[18] + x[19] /\
x[14] = x[19] + x[20] /\
x[15] = x[20] + x[21] 
  
;

output [
 show(x)
];

