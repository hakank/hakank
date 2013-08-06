%
% Crossword problem 'puzzle20, 9 x 9' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 69 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle20, 9 x 9' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (69): puzzle20, 9 x 9
% 
%   * * * _ _ _ * * *
%   * * _ _ _ _ _ * *
%   * _ _ _ _ _ _ _ *
%   _ _ _ _ * _ _ _ _
%   _ _ _ * * * _ _ _
%   _ _ _ _ * _ _ _ _
%   * _ _ _ _ _ _ _ *
%   * * _ _ _ _ _ * *
%   * * * _ _ _ * * *

include "crossword3.mzn"; 

problem_name = "puzzle20, 9 x 9";


% number of letters to assign
N = 52;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3]], words3) 
/\ table([L[4], L[5], L[6], L[7], L[8]], words5) 
/\ table([L[9], L[10], L[11], L[12], L[13], L[14], L[15]], words7) 
/\ table([L[16], L[17], L[18], L[19]], words4) 
/\ table([L[20], L[21], L[22], L[23]], words4) 
/\ table([L[24], L[25], L[26]], words3) 
/\ table([L[27], L[28], L[29]], words3) 
/\ table([L[30], L[31], L[32], L[33]], words4) 
/\ table([L[34], L[35], L[36], L[37]], words4) 
/\ table([L[38], L[39], L[40], L[41], L[42], L[43], L[44]], words7) 
/\ table([L[45], L[46], L[47], L[48], L[49]], words5) 
/\ table([L[50], L[51], L[52]], words3) 
/\ table([L[16], L[24], L[30]], words3) 
/\ table([L[9], L[17], L[25], L[31], L[38]], words5) 
/\ table([L[4], L[10], L[18], L[26], L[32], L[39], L[45]], words7) 
/\ table([L[1], L[5], L[11], L[19]], words4) 
/\ table([L[33], L[40], L[46], L[50]], words4) 
/\ table([L[2], L[6], L[12]], words3) 
/\ table([L[41], L[47], L[51]], words3) 
/\ table([L[3], L[7], L[13], L[20]], words4) 
/\ table([L[34], L[42], L[48], L[52]], words4) 
/\ table([L[8], L[14], L[21], L[27], L[35], L[43], L[49]], words7) 
/\ table([L[15], L[22], L[28], L[36], L[44]], words5) 
/\ table([L[23], L[29], L[37]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 9;
int: num_segments = 24;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,0,0,0,0,0,0,
4,5,6,7,8,0,0,0,0,
9,10,11,12,13,14,15,0,0,
16,17,18,19,0,0,0,0,0,
20,21,22,23,0,0,0,0,0,
24,25,26,0,0,0,0,0,0,
27,28,29,0,0,0,0,0,0,
30,31,32,33,0,0,0,0,0,
34,35,36,37,0,0,0,0,0,
38,39,40,41,42,43,44,0,0,
45,46,47,48,49,0,0,0,0,
50,51,52,0,0,0,0,0,0,
16,24,30,0,0,0,0,0,0,
9,17,25,31,38,0,0,0,0,
4,10,18,26,32,39,45,0,0,
1,5,11,19,0,0,0,0,0,
33,40,46,50,0,0,0,0,0,
2,6,12,0,0,0,0,0,0,
41,47,51,0,0,0,0,0,0,
3,7,13,20,0,0,0,0,0,
34,42,48,52,0,0,0,0,0,
8,14,21,27,35,43,49,0,0,
15,22,28,36,44,0,0,0,0,
23,29,37,0,0,0,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 9;
int: cols = 9;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
0, 0, 0, 1, 2, 3, 0, 0, 0, 
0, 0, 4, 5, 6, 7, 8, 0, 0, 
0, 9, 10, 11, 12, 13, 14, 15, 0, 
16, 17, 18, 19, 0, 20, 21, 22, 23, 
24, 25, 26, 0, 0, 0, 27, 28, 29, 
30, 31, 32, 33, 0, 34, 35, 36, 37, 
0, 38, 39, 40, 41, 42, 43, 44, 0, 
0, 0, 45, 46, 47, 48, 49, 0, 0, 
0, 0, 0, 50, 51, 52, 0, 0, 0, 

]);


