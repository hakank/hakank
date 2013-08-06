%
% Crossword problem '05.10, 5 x 5' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 9 1` ).
%
% This crossword model was 
%
% This is problem '05.10, 5 x 5' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (9): 05.10, 5 x 5
% 
%   * * _ _ _
%   * * _ _ _
%   _ _ _ _ _
%   _ _ _ * *
%   _ _ _ * *

include "crossword3.mzn"; 

problem_name = "05.10, 5 x 5";


% number of letters to assign
N = 17;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3]], words3) 
/\ table([L[4], L[5], L[6]], words3) 
/\ table([L[7], L[8], L[9], L[10], L[11]], words5) 
/\ table([L[12], L[13], L[14]], words3) 
/\ table([L[15], L[16], L[17]], words3) 
/\ table([L[7], L[12], L[15]], words3) 
/\ table([L[8], L[13], L[16]], words3) 
/\ table([L[1], L[4], L[9], L[14], L[17]], words5) 
/\ table([L[2], L[5], L[10]], words3) 
/\ table([L[3], L[6], L[11]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 5;
int: num_segments = 10;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,0,0,
4,5,6,0,0,
7,8,9,10,11,
12,13,14,0,0,
15,16,17,0,0,
7,12,15,0,0,
8,13,16,0,0,
1,4,9,14,17,
2,5,10,0,0,
3,6,11,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 5;
int: cols = 5;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
0, 0, 1, 2, 3, 
0, 0, 4, 5, 6, 
7, 8, 9, 10, 11, 
12, 13, 14, 0, 0, 
15, 16, 17, 0, 0, 

]);


