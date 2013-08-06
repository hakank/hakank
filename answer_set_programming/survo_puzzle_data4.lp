% same as survo_puzzle3.dzn but with hints: difficulty 0
% r = 3;
% c = 4;
% rowsums = [24,15,39];
% colsums = [21,10,18,29];
% matrix = array2d(1..3, 1..4, 
%    [
%      7, 0, 5, 0,
%      0, 1, 0, 8,
%      0, 0, 11, 0
%    ]);

#const r = 3.
#const c = 4.

matrix(1,1,7).
matrix(1,3,5).
matrix(2,2,1).
matrix(2,4,8).
matrix(3,3,11).

rowsums(1,24).
rowsums(2,15).
rowsums(3,39).

colsums(1,21).
colsums(2,10).
colsums(3,18).
colsums(4,29).