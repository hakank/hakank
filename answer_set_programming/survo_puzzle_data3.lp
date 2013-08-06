% http://en.wikipedia.org/wiki/Survo_Puzzle, third example
% difficulty 150 ("open puzzle", i.e. no hints)
% It's an unique solution.
% r = 3;
% c = 4;
% rowsums = [24,15,39];
% colsums = [21,10,18,29];
% matrix = array2d(1..3, 1..4, 
%   [
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0
%   ]);

#const r = 3.
#const c = 4.

% no hints

rowsums(1,24).
rowsums(2,15).
rowsums(3,39).

colsums(1,21).
colsums(2,10).
colsums(3,18).
colsums(4,29).