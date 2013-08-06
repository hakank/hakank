%
% Crossword problem 'puzzle09, 7 x 7' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 58 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle09, 7 x 7' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (58): puzzle09, 7 x 7
% 
%   * * _ _ _ * *
%   * _ _ _ _ _ *
%   _ _ _ * _ _ _
%   _ _ _ _ _ _ _
%   _ _ _ * _ _ _
%   * _ _ _ _ _ *
%   * * _ _ _ * *

include "crossword3.mzn"; 

problem_name = "puzzle09, 7 x 7";


% number of letters to assign
N = 35;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3]], words3) 
/\ table([L[4], L[5], L[6], L[7], L[8]], words5) 
/\ table([L[9], L[10], L[11]], words3) 
/\ table([L[12], L[13], L[14]], words3) 
/\ table([L[15], L[16], L[17], L[18], L[19], L[20], L[21]], words7) 
/\ table([L[22], L[23], L[24]], words3) 
/\ table([L[25], L[26], L[27]], words3) 
/\ table([L[28], L[29], L[30], L[31], L[32]], words5) 
/\ table([L[33], L[34], L[35]], words3) 
/\ table([L[9], L[15], L[22]], words3) 
/\ table([L[4], L[10], L[16], L[23], L[28]], words5) 
/\ table([L[1], L[5], L[11], L[17], L[24], L[29], L[33]], words7) 
/\ table([L[2], L[6]], words2) 
/\ table([L[18]], words1) 
/\ table([L[30], L[34]], words2) 
/\ table([L[3], L[7], L[12], L[19], L[25], L[31], L[35]], words7) 
/\ table([L[8], L[13], L[20], L[26], L[32]], words5) 
/\ table([L[14], L[21], L[27]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 7;
int: num_segments = 18;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,0,0,0,0,
4,5,6,7,8,0,0,
9,10,11,0,0,0,0,
12,13,14,0,0,0,0,
15,16,17,18,19,20,21,
22,23,24,0,0,0,0,
25,26,27,0,0,0,0,
28,29,30,31,32,0,0,
33,34,35,0,0,0,0,
9,15,22,0,0,0,0,
4,10,16,23,28,0,0,
1,5,11,17,24,29,33,
2,6,0,0,0,0,0,
18,0,0,0,0,0,0,
30,34,0,0,0,0,0,
3,7,12,19,25,31,35,
8,13,20,26,32,0,0,
14,21,27,0,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 7;
int: cols = 7;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
0, 0, 1, 2, 3, 0, 0, 
0, 4, 5, 6, 7, 8, 0, 
9, 10, 11, 0, 12, 13, 14, 
15, 16, 17, 18, 19, 20, 21, 
22, 23, 24, 0, 25, 26, 27, 
0, 28, 29, 30, 31, 32, 0, 
0, 0, 33, 34, 35, 0, 0, 

]);


