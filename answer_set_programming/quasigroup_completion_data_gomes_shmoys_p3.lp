% Example from Gomes & Shmoys, page 3.
% Solution:
% 4 1 2 3
% 2 3 4 1
% 1 4 3 2
% 3 2 1 4
%
% _, 1, 2, 3,
% 2, _, 4, 1, 
% 1, 4, _, 2,
% 3, _, 1, _

#const n = 4.

cell(1, 2, 1).
cell(1, 3, 2).
cell(1, 4, 3).
cell(2, 1, 2).
cell(2, 3, 4).
cell(2, 4, 1).
cell(3, 1, 1).
cell(3, 2, 4).
cell(3, 4, 2).
cell(4, 1, 3).
cell(4, 3, 1).
