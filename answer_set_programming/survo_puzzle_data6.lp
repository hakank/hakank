% http://www.survo.fi/puzzles/280708.txt, third puzzle
% Survo puzzle 128/2008 (1700) #364-35846
%
%    A  B  C  D  E  F
% 1  *  *  *  *  *  * 30
% 2  *  * 18  *  *  * 86
% 3  *  *  *  *  *  * 55
%   22 11 42 32 27 37
% r = 3;
% c = 6;
% rowsums = [30, 86, 55];
% colsums = [22, 11, 42, 32, 27, 37];
% matrix = array2d(1..3, 1..6, 
%   [
%     0, 0,  0, 0, 0, 0,
%     0, 0, 18, 0, 0, 0,
%     0, 0,  0, 0, 0, 0
%   ]);


#const r = 3.
#const c = 6.

matrix(2,3,18).

rowsums(1,30).
rowsums(2,86).
rowsums(3,55).

colsums(1,22).
colsums(2,11).
colsums(3,42).
colsums(4,32).
colsums(5,27).
colsums(6,37).

