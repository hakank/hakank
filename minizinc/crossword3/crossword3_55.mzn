%
% Crossword problem 'puzzle06, 5 x 5' in MiniZinc.
%
% (generated Sat Sep 24 08:54:24 2011 by `make_crossword.pl 55 1` ).
%
% This crossword model was 
%
% This is problem 'puzzle06, 5 x 5' from the Gecode crossword example:
%   http://www.gecode.org/doc-latest/reference/crossword_8cpp_source.html
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Gecode problem to solve (55): puzzle06, 5 x 5
% 
%   _ _ _ _ _
%   _ _ _ * _
%   _ _ _ _ _
%   _ * _ _ _
%   _ _ _ _ _

include "crossword3.mzn"; 

problem_name = "puzzle06, 5 x 5";


% number of letters to assign
N = 23;


% distinct words
require_distinct_words = 1;

constraint
  table([L[1], L[2], L[3], L[4], L[5]], words5) 
/\ table([L[6], L[7], L[8]], words3) 
/\ table([L[9]], words1) 
/\ table([L[10], L[11], L[12], L[13], L[14]], words5) 
/\ table([L[15]], words1) 
/\ table([L[16], L[17], L[18]], words3) 
/\ table([L[19], L[20], L[21], L[22], L[23]], words5) 
/\ table([L[1], L[6], L[10], L[15], L[19]], words5) 
/\ table([L[2], L[7], L[11]], words3) 
/\ table([L[20]], words1) 
/\ table([L[3], L[8], L[12], L[16], L[21]], words5) 
/\ table([L[4]], words1) 
/\ table([L[13], L[17], L[22]], words3) 
/\ table([L[5], L[9], L[14], L[18], L[23]], words5)
;

% The segments (words)
% (0 for fill outs)
int: max_length = 5;
int: num_segments = 14;
array[1..num_segments, 1..max_length] of int: segments = array2d(1..num_segments, 1..max_length, 
[
1,2,3,4,5,
6,7,8,0,0,
9,0,0,0,0,
10,11,12,13,14,
15,0,0,0,0,
16,17,18,0,0,
19,20,21,22,23,
1,6,10,15,19,
2,7,11,0,0,
20,0,0,0,0,
3,8,12,16,21,
4,0,0,0,0,
13,17,22,0,0,
5,9,14,18,23,

]);

% Problem where each cell is assigned a unique index.
% (0 means blocked cell)
int: rows = 5;
int: cols = 5;
array[1..rows, 1..cols] of int: problem = array2d(1..rows, 1..cols,
[
1, 2, 3, 4, 5, 
6, 7, 8, 0, 9, 
10, 11, 12, 13, 14, 
15, 0, 16, 17, 18, 
19, 20, 21, 22, 23, 

]);


