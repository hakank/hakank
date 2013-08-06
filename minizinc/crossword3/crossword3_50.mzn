%
% Crossword problem 'puzzle01, 2 x 2' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 50 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle01, 2 x 2' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (50): puzzle01, 2 x 2
% 
%   _ *
%   _ _

include "crossword3.mzn"; 

problem_name = "puzzle01, 2 x 2";


% number of letters to assign
N = 3;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1]], words1) 
/\ table([L[2], L[3]], words2) 
/\ table([L[1], L[2]], words2) 
/\ table([L[3]], words1)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 2;
int: num_segments = 4;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,0,
2,3,
1,2,
3,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 2;
int: cols = 2;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 0, 
2, 3, 

]);


