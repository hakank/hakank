%
% Crossword problem 'puzzle08, 7 x 7' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 57 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle08, 7 x 7' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (57): puzzle08, 7 x 7
% 
%   _ _ _ _ * _ _
%   _ _ _ * _ _ _
%   _ _ * _ _ _ *
%   _ _ _ _ _ _ _
%   * _ _ _ * _ _
%   _ _ _ * _ _ _
%   _ _ * _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle08, 7 x 7";


% number of letters to assign
N = 41;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4]], words4) 
/\ table([L[5], L[6]], words2) 
/\ table([L[7], L[8], L[9]], words3) 
/\ table([L[10], L[11], L[12]], words3) 
/\ table([L[13], L[14]], words2) 
/\ table([L[15], L[16], L[17]], words3) 
/\ table([L[18], L[19], L[20], L[21], L[22], L[23], L[24]], words7) 
/\ table([L[25], L[26], L[27]], words3) 
/\ table([L[28], L[29]], words2) 
/\ table([L[30], L[31], L[32]], words3) 
/\ table([L[33], L[34], L[35]], words3) 
/\ table([L[36], L[37]], words2) 
/\ table([L[38], L[39], L[40], L[41]], words4) 
/\ table([L[1], L[7], L[13], L[18]], words4) 
/\ table([L[30], L[36]], words2) 
/\ table([L[2], L[8], L[14], L[19], L[25], L[31], L[37]], words7) 
/\ table([L[3], L[9]], words2) 
/\ table([L[20], L[26], L[32]], words3) 
/\ table([L[4]], words1) 
/\ table([L[15], L[21], L[27]], words3) 
/\ table([L[38]], words1) 
/\ table([L[10], L[16], L[22]], words3) 
/\ table([L[33], L[39]], words2) 
/\ table([L[5], L[11], L[17], L[23], L[28], L[34], L[40]], words7) 
/\ table([L[6], L[12]], words2) 
/\ table([L[24], L[29], L[35], L[41]], words4)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 7;
int: num_segments = 26;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,0,0,0,
5,6,0,0,0,0,0,
7,8,9,0,0,0,0,
10,11,12,0,0,0,0,
13,14,0,0,0,0,0,
15,16,17,0,0,0,0,
18,19,20,21,22,23,24,
25,26,27,0,0,0,0,
28,29,0,0,0,0,0,
30,31,32,0,0,0,0,
33,34,35,0,0,0,0,
36,37,0,0,0,0,0,
38,39,40,41,0,0,0,
1,7,13,18,0,0,0,
30,36,0,0,0,0,0,
2,8,14,19,25,31,37,
3,9,0,0,0,0,0,
20,26,32,0,0,0,0,
4,0,0,0,0,0,0,
15,21,27,0,0,0,0,
38,0,0,0,0,0,0,
10,16,22,0,0,0,0,
33,39,0,0,0,0,0,
5,11,17,23,28,34,40,
6,12,0,0,0,0,0,
24,29,35,41,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 7;
int: cols = 7;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 0, 5, 6, 
7, 8, 9, 0, 10, 11, 12, 
13, 14, 0, 15, 16, 17, 0, 
18, 19, 20, 21, 22, 23, 24, 
0, 25, 26, 27, 0, 28, 29, 
30, 31, 32, 0, 33, 34, 35, 
36, 37, 0, 38, 39, 40, 41, 

]);


