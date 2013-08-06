% http://en.wikipedia.org/wiki/Survo_Puzzle, second example
% difficulty 0
% r = 2;
% c = 3;
% rowsums = [9,12];
% colsums = [9, 7, 5];
% matrix = array2d(1..2, 1..3, 
%    [
%      0, 0, 3,
%      0, 6, 0
%    ]);

#const r = 2.
#const c = 3.

matrix(1,3,3).
matrix(2,2,6).

rowsums(1,9).
rowsums(2,12).

colsums(1,9).
colsums(2,7).
colsums(3,5).
