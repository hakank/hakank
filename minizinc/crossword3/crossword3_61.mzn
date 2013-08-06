%
% Crossword problem 'puzzle12, 8 x 8' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 61 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle12, 8 x 8' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (61): puzzle12, 8 x 8
% 
%   _ _ _ _ * _ _ _
%   _ _ _ _ * _ _ _
%   _ _ _ _ * _ _ _
%   * * * _ _ _ _ _
%   _ _ _ _ _ * * *
%   _ _ _ * _ _ _ _
%   _ _ _ * _ _ _ _
%   _ _ _ * _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle12, 8 x 8";


% number of letters to assign
N = 52;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4]], words4) 
/\ table([L[5], L[6], L[7]], words3) 
/\ table([L[8], L[9], L[10], L[11]], words4) 
/\ table([L[12], L[13], L[14]], words3) 
/\ table([L[15], L[16], L[17], L[18]], words4) 
/\ table([L[19], L[20], L[21]], words3) 
/\ table([L[22], L[23], L[24], L[25], L[26]], words5) 
/\ table([L[27], L[28], L[29], L[30], L[31]], words5) 
/\ table([L[32], L[33], L[34]], words3) 
/\ table([L[35], L[36], L[37], L[38]], words4) 
/\ table([L[39], L[40], L[41]], words3) 
/\ table([L[42], L[43], L[44], L[45]], words4) 
/\ table([L[46], L[47], L[48]], words3) 
/\ table([L[49], L[50], L[51], L[52]], words4) 
/\ table([L[1], L[8], L[15]], words3) 
/\ table([L[27], L[32], L[39], L[46]], words4) 
/\ table([L[2], L[9], L[16]], words3) 
/\ table([L[28], L[33], L[40], L[47]], words4) 
/\ table([L[3], L[10], L[17]], words3) 
/\ table([L[29], L[34], L[41], L[48]], words4) 
/\ table([L[4], L[11], L[18], L[22], L[30]], words5) 
/\ table([L[23], L[31], L[35], L[42], L[49]], words5) 
/\ table([L[5], L[12], L[19], L[24]], words4) 
/\ table([L[36], L[43], L[50]], words3) 
/\ table([L[6], L[13], L[20], L[25]], words4) 
/\ table([L[37], L[44], L[51]], words3) 
/\ table([L[7], L[14], L[21], L[26]], words4) 
/\ table([L[38], L[45], L[52]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 8;
int: num_segments = 28;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,0,0,0,0,
5,6,7,0,0,0,0,0,
8,9,10,11,0,0,0,0,
12,13,14,0,0,0,0,0,
15,16,17,18,0,0,0,0,
19,20,21,0,0,0,0,0,
22,23,24,25,26,0,0,0,
27,28,29,30,31,0,0,0,
32,33,34,0,0,0,0,0,
35,36,37,38,0,0,0,0,
39,40,41,0,0,0,0,0,
42,43,44,45,0,0,0,0,
46,47,48,0,0,0,0,0,
49,50,51,52,0,0,0,0,
1,8,15,0,0,0,0,0,
27,32,39,46,0,0,0,0,
2,9,16,0,0,0,0,0,
28,33,40,47,0,0,0,0,
3,10,17,0,0,0,0,0,
29,34,41,48,0,0,0,0,
4,11,18,22,30,0,0,0,
23,31,35,42,49,0,0,0,
5,12,19,24,0,0,0,0,
36,43,50,0,0,0,0,0,
6,13,20,25,0,0,0,0,
37,44,51,0,0,0,0,0,
7,14,21,26,0,0,0,0,
38,45,52,0,0,0,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 8;
int: cols = 8;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 0, 5, 6, 7, 
8, 9, 10, 11, 0, 12, 13, 14, 
15, 16, 17, 18, 0, 19, 20, 21, 
0, 0, 0, 22, 23, 24, 25, 26, 
27, 28, 29, 30, 31, 0, 0, 0, 
32, 33, 34, 0, 35, 36, 37, 38, 
39, 40, 41, 0, 42, 43, 44, 45, 
46, 47, 48, 0, 49, 50, 51, 52, 

]);


