%
% Chain Sudoku / Strimko
% From
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/sudoku/chain
%
%  Chain Sudoku 9x9
%  Difficulty: Very hard
%  Code: 069.9x9
%  ID: 06990000006
%
#const n = 9.

%
% streams
% 
% 1,1,2,2,2,2,3,3,3,
% 1,1,2,2,2,3,2,3,4,
% 1,1,2,3,3,3,3,4,4,
% 1,5,6,6,6,7,7,7,4,
% 1,5,5,6,6,6,7,4,7,
% 1,5,5,5,5,7,6,7,4,
% 8,5,5,9,7,7,6,4,4,
% 8,8,8,9,9,9,6,9,4,
% 8,8,8,8,8,9,9,9,9
%
streams(1, 1, 1).
streams(1, 2, 1).
streams(1, 3, 2).
streams(1, 4, 2).
streams(1, 5, 2).
streams(1, 6, 2).
streams(1, 7, 3).
streams(1, 8, 3).
streams(1, 9, 3).
streams(2, 1, 1).
streams(2, 2, 1).
streams(2, 3, 2).
streams(2, 4, 2).
streams(2, 5, 2).
streams(2, 6, 3).
streams(2, 7, 2).
streams(2, 8, 3).
streams(2, 9, 4).
streams(3, 1, 1).
streams(3, 2, 1).
streams(3, 3, 2).
streams(3, 4, 3).
streams(3, 5, 3).
streams(3, 6, 3).
streams(3, 7, 3).
streams(3, 8, 4).
streams(3, 9, 4).
streams(4, 1, 1).
streams(4, 2, 5).
streams(4, 3, 6).
streams(4, 4, 6).
streams(4, 5, 6).
streams(4, 6, 7).
streams(4, 7, 7).
streams(4, 8, 7).
streams(4, 9, 4).
streams(5, 1, 1).
streams(5, 2, 5).
streams(5, 3, 5).
streams(5, 4, 6).
streams(5, 5, 6).
streams(5, 6, 6).
streams(5, 7, 7).
streams(5, 8, 4).
streams(5, 9, 7).
streams(6, 1, 1).
streams(6, 2, 5).
streams(6, 3, 5).
streams(6, 4, 5).
streams(6, 5, 5).
streams(6, 6, 7).
streams(6, 7, 6).
streams(6, 8, 7).
streams(6, 9, 4).
streams(7, 1, 8).
streams(7, 2, 5).
streams(7, 3, 5).
streams(7, 4, 9).
streams(7, 5, 7).
streams(7, 6, 7).
streams(7, 7, 6).
streams(7, 8, 4).
streams(7, 9, 4).
streams(8, 1, 8).
streams(8, 2, 8).
streams(8, 3, 8).
streams(8, 4, 9).
streams(8, 5, 9).
streams(8, 6, 9).
streams(8, 7, 6).
streams(8, 8, 9).
streams(8, 9, 4).
streams(9, 1, 8).
streams(9, 2, 8).
streams(9, 3, 8).
streams(9, 4, 8).
streams(9, 5, 8).
streams(9, 6, 9).
streams(9, 7, 9).
streams(9, 8, 9).
streams(9, 9, 9).

%
% placed
% 
placed(1,5,8).
placed(1,8,2).
placed(2,1,4).
placed(2,3,7).
placed(3,5,6).
placed(3,8,7).
placed(4,4,9).
placed(4,6,2).
placed(5,1,8).
placed(5,3,1).
placed(5,5,5).
placed(5,7,6).
placed(5,9,3).
placed(6,4,5).
placed(6,6,7).
placed(7,2,6).
placed(7,5,4).
placed(8,7,8).
placed(8,9,5).
placed(9,2,8).
placed(9,5,7).
