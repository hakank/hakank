% http://en.wikipedia.org/wiki/Survo_Puzzle, first example
% rowsums = [30,18,30]
% colsums = [27,16,10,25]
% hints:
%     0, 6, 0, 0,
%     8, 0, 0, 0,
%     0, 0, 3, 0 
% r = 3
% c = 4

#const r = 3.
#const c = 4.

% rowsums(30,18,30).
% colsums(27,16,10,25).
rowsums(1,30).
rowsums(2,18).
rowsums(3,30).

colsums(1,27).
colsums(2,16).
colsums(3,10).
colsums(4,25).

% hints:
matrix(1,2,6).
matrix(2,1,8).
matrix(3,3,3).
