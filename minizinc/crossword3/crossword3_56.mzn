%
% Crossword problem 'puzzle07, 6 x 6' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 56 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle07, 6 x 6' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (56): puzzle07, 6 x 6
% 
%   _ _ _ _ _ *
%   _ * _ _ _ _
%   _ _ _ * _ _
%   _ _ * _ _ _
%   _ _ _ _ * _
%   * _ _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle07, 6 x 6";


% number of letters to assign
N = 30;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4], L[5]], words5) 
/\ table([L[6]], words1) 
/\ table([L[7], L[8], L[9], L[10]], words4) 
/\ table([L[11], L[12], L[13]], words3) 
/\ table([L[14], L[15]], words2) 
/\ table([L[16], L[17]], words2) 
/\ table([L[18], L[19], L[20]], words3) 
/\ table([L[21], L[22], L[23], L[24]], words4) 
/\ table([L[25]], words1) 
/\ table([L[26], L[27], L[28], L[29], L[30]], words5) 
/\ table([L[1], L[6], L[11], L[16], L[21]], words5) 
/\ table([L[2]], words1) 
/\ table([L[12], L[17], L[22], L[26]], words4) 
/\ table([L[3], L[7], L[13]], words3) 
/\ table([L[23], L[27]], words2) 
/\ table([L[4], L[8]], words2) 
/\ table([L[18], L[24], L[28]], words3) 
/\ table([L[5], L[9], L[14], L[19]], words4) 
/\ table([L[29]], words1) 
/\ table([L[10], L[15], L[20], L[25], L[30]], words5)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 6;
int: num_segments = 20;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,5,0,
6,0,0,0,0,0,
7,8,9,10,0,0,
11,12,13,0,0,0,
14,15,0,0,0,0,
16,17,0,0,0,0,
18,19,20,0,0,0,
21,22,23,24,0,0,
25,0,0,0,0,0,
26,27,28,29,30,0,
1,6,11,16,21,0,
2,0,0,0,0,0,
12,17,22,26,0,0,
3,7,13,0,0,0,
23,27,0,0,0,0,
4,8,0,0,0,0,
18,24,28,0,0,0,
5,9,14,19,0,0,
29,0,0,0,0,0,
10,15,20,25,30,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 6;
int: cols = 6;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 5, 0, 
6, 0, 7, 8, 9, 10, 
11, 12, 13, 0, 14, 15, 
16, 17, 0, 18, 19, 20, 
21, 22, 23, 24, 0, 25, 
0, 26, 27, 28, 29, 30, 

]);


