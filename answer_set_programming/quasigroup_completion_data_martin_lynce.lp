% Example from Ruben Martins and Inès Lynce
% Breaking Local Symmetries in Quasigroup Completion Problems, page 3
% Problem:
%  1, _, _, _, 4,
%  _, 5, _, _, _,
%  4, _, _, 2, _,
%  _, 4, _, _, _,
%  _, _, 5, _, 1
%
% The solution is unique:
% 1 3 2 5 4
% 2 5 4 1 3
% 4 1 3 2 5
% 5 4 1 3 2
% 3 2 5 4 1
%

#const n = 5.

%
% data
%
cell(1,1,1).
cell(1,5,4).
cell(2,2,5).
cell(3,1,4).
cell(3,4,2).
cell(4,2,4).
cell(5,3,5).
cell(5,5,1).
