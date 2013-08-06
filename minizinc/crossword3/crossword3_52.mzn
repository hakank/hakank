%
% Crossword problem 'puzzle03, 4 x 4' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 52 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle03, 4 x 4' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (52): puzzle03, 4 x 4
% 
%   _ _ _ *
%   _ _ _ _
%   _ _ _ _
%   * _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle03, 4 x 4";


% number of letters to assign
N = 14;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3]], words3) 
/\ table([L[4], L[5], L[6], L[7]], words4) 
/\ table([L[8], L[9], L[10], L[11]], words4) 
/\ table([L[12], L[13], L[14]], words3) 
/\ table([L[1], L[4], L[8]], words3) 
/\ table([L[2], L[5], L[9], L[12]], words4) 
/\ table([L[3], L[6], L[10], L[13]], words4) 
/\ table([L[7], L[11], L[14]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 4;
int: num_segments = 8;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,0,
4,5,6,7,
8,9,10,11,
12,13,14,0,
1,4,8,0,
2,5,9,12,
3,6,10,13,
7,11,14,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 4;
int: cols = 4;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 0, 
4, 5, 6, 7, 
8, 9, 10, 11, 
0, 12, 13, 14, 

]);


