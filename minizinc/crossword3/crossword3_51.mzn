%
% Crossword problem 'puzzle02, 3 x 3' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 51 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle02, 3 x 3' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (51): puzzle02, 3 x 3
% 
%   * _ _
%   _ _ _
%   _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle02, 3 x 3";


% number of letters to assign
N = 8;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2]], words2) 
/\ table([L[3], L[4], L[5]], words3) 
/\ table([L[6], L[7], L[8]], words3) 
/\ table([L[3], L[6]], words2) 
/\ table([L[1], L[4], L[7]], words3) 
/\ table([L[2], L[5], L[8]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 3;
int: num_segments = 6;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,0,
3,4,5,
6,7,8,
3,6,0,
1,4,7,
2,5,8,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 3;
int: cols = 3;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
0, 1, 2, 
3, 4, 5, 
6, 7, 8, 

]);


