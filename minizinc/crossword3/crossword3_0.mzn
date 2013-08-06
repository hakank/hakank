%
% Crossword problem '05.01, 5 x 5' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 0 1` ).
%
% This crossword model was 
%
% This is problem '05.01, 5 x 5' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (0): 05.01, 5 x 5
% 
%   _ _ _ _ _
%   _ _ _ _ _
%   _ _ _ _ _
%   _ _ _ _ _
%   _ _ _ _ _

include "crossword3.mzn"; 

problem_name = "05.01, 5 x 5";


% number of letters to assign
N = 25;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4], L[5]], words5) 
/\ table([L[6], L[7], L[8], L[9], L[10]], words5) 
/\ table([L[11], L[12], L[13], L[14], L[15]], words5) 
/\ table([L[16], L[17], L[18], L[19], L[20]], words5) 
/\ table([L[21], L[22], L[23], L[24], L[25]], words5) 
/\ table([L[1], L[6], L[11], L[16], L[21]], words5) 
/\ table([L[2], L[7], L[12], L[17], L[22]], words5) 
/\ table([L[3], L[8], L[13], L[18], L[23]], words5) 
/\ table([L[4], L[9], L[14], L[19], L[24]], words5) 
/\ table([L[5], L[10], L[15], L[20], L[25]], words5)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 5;
int: num_segments = 10;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,5,
6,7,8,9,10,
11,12,13,14,15,
16,17,18,19,20,
21,22,23,24,25,
1,6,11,16,21,
2,7,12,17,22,
3,8,13,18,23,
4,9,14,19,24,
5,10,15,20,25,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 5;
int: cols = 5;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 5, 
6, 7, 8, 9, 10, 
11, 12, 13, 14, 15, 
16, 17, 18, 19, 20, 
21, 22, 23, 24, 25, 

]);


