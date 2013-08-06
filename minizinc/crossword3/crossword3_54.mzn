%
% Crossword problem 'puzzle05, 5 x 5' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 54 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle05, 5 x 5' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (54): puzzle05, 5 x 5
% 
%   _ _ _ _ *
%   _ _ _ * _
%   _ _ _ _ _
%   _ * _ _ _
%   * _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle05, 5 x 5";


% number of letters to assign
N = 21;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4]], words4) 
/\ table([L[5], L[6], L[7]], words3) 
/\ table([L[8]], words1) 
/\ table([L[9], L[10], L[11], L[12], L[13]], words5) 
/\ table([L[14]], words1) 
/\ table([L[15], L[16], L[17]], words3) 
/\ table([L[18], L[19], L[20], L[21]], words4) 
/\ table([L[1], L[5], L[9], L[14]], words4) 
/\ table([L[2], L[6], L[10]], words3) 
/\ table([L[18]], words1) 
/\ table([L[3], L[7], L[11], L[15], L[19]], words5) 
/\ table([L[4]], words1) 
/\ table([L[12], L[16], L[20]], words3) 
/\ table([L[8], L[13], L[17], L[21]], words4)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 5;
int: num_segments = 14;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,0,
5,6,7,0,0,
8,0,0,0,0,
9,10,11,12,13,
14,0,0,0,0,
15,16,17,0,0,
18,19,20,21,0,
1,5,9,14,0,
2,6,10,0,0,
18,0,0,0,0,
3,7,11,15,19,
4,0,0,0,0,
12,16,20,0,0,
8,13,17,21,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 5;
int: cols = 5;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 0, 
5, 6, 7, 0, 8, 
9, 10, 11, 12, 13, 
14, 0, 15, 16, 17, 
0, 18, 19, 20, 21, 

]);


