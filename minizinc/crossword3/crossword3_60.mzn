%
% Crossword problem 'puzzle11, 7 x 7' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 60 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle11, 7 x 7' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (60): puzzle11, 7 x 7
% 
%   * * _ _ _ _ *
%   * _ _ _ _ _ _
%   _ _ _ * _ _ _
%   _ _ _ * _ _ _
%   _ _ _ * _ _ _
%   _ _ _ _ _ _ *
%   * _ _ _ _ * *

include "crossword3.mzn"; 

problem_name = "puzzle11, 7 x 7";


% number of letters to assign
N = 38;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4]], words4) 
/\ table([L[5], L[6], L[7], L[8], L[9], L[10]], words6) 
/\ table([L[11], L[12], L[13]], words3) 
/\ table([L[14], L[15], L[16]], words3) 
/\ table([L[17], L[18], L[19]], words3) 
/\ table([L[20], L[21], L[22]], words3) 
/\ table([L[23], L[24], L[25]], words3) 
/\ table([L[26], L[27], L[28]], words3) 
/\ table([L[29], L[30], L[31], L[32], L[33], L[34]], words6) 
/\ table([L[35], L[36], L[37], L[38]], words4) 
/\ table([L[11], L[17], L[23], L[29]], words4) 
/\ table([L[5], L[12], L[18], L[24], L[30], L[35]], words6) 
/\ table([L[1], L[6], L[13], L[19], L[25], L[31], L[36]], words7) 
/\ table([L[2], L[7]], words2) 
/\ table([L[32], L[37]], words2) 
/\ table([L[3], L[8], L[14], L[20], L[26], L[33], L[38]], words7) 
/\ table([L[4], L[9], L[15], L[21], L[27], L[34]], words6) 
/\ table([L[10], L[16], L[22], L[28]], words4)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 7;
int: num_segments = 18;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,0,0,0,
5,6,7,8,9,10,0,
11,12,13,0,0,0,0,
14,15,16,0,0,0,0,
17,18,19,0,0,0,0,
20,21,22,0,0,0,0,
23,24,25,0,0,0,0,
26,27,28,0,0,0,0,
29,30,31,32,33,34,0,
35,36,37,38,0,0,0,
11,17,23,29,0,0,0,
5,12,18,24,30,35,0,
1,6,13,19,25,31,36,
2,7,0,0,0,0,0,
32,37,0,0,0,0,0,
3,8,14,20,26,33,38,
4,9,15,21,27,34,0,
10,16,22,28,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 7;
int: cols = 7;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
0, 0, 1, 2, 3, 4, 0, 
0, 5, 6, 7, 8, 9, 10, 
11, 12, 13, 0, 14, 15, 16, 
17, 18, 19, 0, 20, 21, 22, 
23, 24, 25, 0, 26, 27, 28, 
29, 30, 31, 32, 33, 34, 0, 
0, 35, 36, 37, 38, 0, 0, 

]);


