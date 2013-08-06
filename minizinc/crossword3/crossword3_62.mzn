%
% Crossword problem 'puzzle13, 9 x 9' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 62 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle13, 9 x 9' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (62): puzzle13, 9 x 9
% 
%   _ _ _ _ * _ _ _ _
%   _ _ _ _ * _ _ _ _
%   _ _ _ * * * _ _ _
%   _ _ _ _ _ _ _ _ _
%   * * * _ _ _ * * *
%   _ _ _ _ _ _ _ _ _
%   _ _ _ * * * _ _ _
%   _ _ _ _ * _ _ _ _
%   _ _ _ _ * _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle13, 9 x 9";


% number of letters to assign
N = 65;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4]], words4) 
/\ table([L[5], L[6], L[7], L[8]], words4) 
/\ table([L[9], L[10], L[11], L[12]], words4) 
/\ table([L[13], L[14], L[15], L[16]], words4) 
/\ table([L[17], L[18], L[19]], words3) 
/\ table([L[20], L[21], L[22]], words3) 
/\ table([L[23], L[24], L[25], L[26], L[27], L[28], L[29], L[30], L[31]], words9) 
/\ table([L[32], L[33], L[34]], words3) 
/\ table([L[35], L[36], L[37], L[38], L[39], L[40], L[41], L[42], L[43]], words9) 
/\ table([L[44], L[45], L[46]], words3) 
/\ table([L[47], L[48], L[49]], words3) 
/\ table([L[50], L[51], L[52], L[53]], words4) 
/\ table([L[54], L[55], L[56], L[57]], words4) 
/\ table([L[58], L[59], L[60], L[61]], words4) 
/\ table([L[62], L[63], L[64], L[65]], words4) 
/\ table([L[1], L[9], L[17], L[23]], words4) 
/\ table([L[35], L[44], L[50], L[58]], words4) 
/\ table([L[2], L[10], L[18], L[24]], words4) 
/\ table([L[36], L[45], L[51], L[59]], words4) 
/\ table([L[3], L[11], L[19], L[25]], words4) 
/\ table([L[37], L[46], L[52], L[60]], words4) 
/\ table([L[4], L[12]], words2) 
/\ table([L[26], L[32], L[38]], words3) 
/\ table([L[53], L[61]], words2) 
/\ table([L[27], L[33], L[39]], words3) 
/\ table([L[5], L[13]], words2) 
/\ table([L[28], L[34], L[40]], words3) 
/\ table([L[54], L[62]], words2) 
/\ table([L[6], L[14], L[20], L[29]], words4) 
/\ table([L[41], L[47], L[55], L[63]], words4) 
/\ table([L[7], L[15], L[21], L[30]], words4) 
/\ table([L[42], L[48], L[56], L[64]], words4) 
/\ table([L[8], L[16], L[22], L[31]], words4) 
/\ table([L[43], L[49], L[57], L[65]], words4)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 9;
int: num_segments = 34;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,0,0,0,0,0,
5,6,7,8,0,0,0,0,0,
9,10,11,12,0,0,0,0,0,
13,14,15,16,0,0,0,0,0,
17,18,19,0,0,0,0,0,0,
20,21,22,0,0,0,0,0,0,
23,24,25,26,27,28,29,30,31,
32,33,34,0,0,0,0,0,0,
35,36,37,38,39,40,41,42,43,
44,45,46,0,0,0,0,0,0,
47,48,49,0,0,0,0,0,0,
50,51,52,53,0,0,0,0,0,
54,55,56,57,0,0,0,0,0,
58,59,60,61,0,0,0,0,0,
62,63,64,65,0,0,0,0,0,
1,9,17,23,0,0,0,0,0,
35,44,50,58,0,0,0,0,0,
2,10,18,24,0,0,0,0,0,
36,45,51,59,0,0,0,0,0,
3,11,19,25,0,0,0,0,0,
37,46,52,60,0,0,0,0,0,
4,12,0,0,0,0,0,0,0,
26,32,38,0,0,0,0,0,0,
53,61,0,0,0,0,0,0,0,
27,33,39,0,0,0,0,0,0,
5,13,0,0,0,0,0,0,0,
28,34,40,0,0,0,0,0,0,
54,62,0,0,0,0,0,0,0,
6,14,20,29,0,0,0,0,0,
41,47,55,63,0,0,0,0,0,
7,15,21,30,0,0,0,0,0,
42,48,56,64,0,0,0,0,0,
8,16,22,31,0,0,0,0,0,
43,49,57,65,0,0,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 9;
int: cols = 9;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 0, 5, 6, 7, 8, 
9, 10, 11, 12, 0, 13, 14, 15, 16, 
17, 18, 19, 0, 0, 0, 20, 21, 22, 
23, 24, 25, 26, 27, 28, 29, 30, 31, 
0, 0, 0, 32, 33, 34, 0, 0, 0, 
35, 36, 37, 38, 39, 40, 41, 42, 43, 
44, 45, 46, 0, 0, 0, 47, 48, 49, 
50, 51, 52, 53, 0, 54, 55, 56, 57, 
58, 59, 60, 61, 0, 62, 63, 64, 65, 

]);


