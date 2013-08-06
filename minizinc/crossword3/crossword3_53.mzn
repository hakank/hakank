%
% Crossword problem 'puzzle04, 5 x 5' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 53 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle04, 5 x 5' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (53): puzzle04, 5 x 5
% 
%   _ _ _ * *
%   _ _ _ _ *
%   _ _ _ _ _
%   * _ _ _ _
%   * * _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle04, 5 x 5";


% number of letters to assign
N = 19;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3]], words3) 
/\ table([L[4], L[5], L[6], L[7]], words4) 
/\ table([L[8], L[9], L[10], L[11], L[12]], words5) 
/\ table([L[13], L[14], L[15], L[16]], words4) 
/\ table([L[17], L[18], L[19]], words3) 
/\ table([L[1], L[4], L[8]], words3) 
/\ table([L[2], L[5], L[9], L[13]], words4) 
/\ table([L[3], L[6], L[10], L[14], L[17]], words5) 
/\ table([L[7], L[11], L[15], L[18]], words4) 
/\ table([L[12], L[16], L[19]], words3)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 5;
int: num_segments = 10;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,0,0,
4,5,6,7,0,
8,9,10,11,12,
13,14,15,16,0,
17,18,19,0,0,
1,4,8,0,0,
2,5,9,13,0,
3,6,10,14,17,
7,11,15,18,0,
12,16,19,0,0,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 5;
int: cols = 5;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 0, 0, 
4, 5, 6, 7, 0, 
8, 9, 10, 11, 12, 
0, 13, 14, 15, 16, 
0, 0, 17, 18, 19, 

]);


