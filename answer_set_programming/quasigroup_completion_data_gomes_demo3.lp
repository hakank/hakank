% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #3. 
% Coding:
%    dark red   = 1
%    light blue = 2 
%    dark blue  = 3 
%    light red  = 4
%    brown      = 5
%    green      = 6
%    pink       = 7
%    grey       = 8
%    black      = 9
%    yellow     = 10    
% There are 40944 solutions for this pattern.
% _, _, 1, 5, 2, 6, 7, 8, _, _,
% _, 1, 5, 2, _, _, 6, 7, 8, _,
% 1, 5, 2, _, _, _, _, 6, 7, 8,
% 5, 2, _, _, _, _, _, _, 6, 7,
% 2, _, _, _, _, _, _, _, _, 6,
% 4,10, _, _, _, _, _, _, 3, 9,
% _, 4,10, _, _, _, _, 3, 9, _,
% _, _, 4,10, _, _, 3, 9, _, _,
% _, _, _, 4,10, 3, 9, _, _, _, 
% _, _, _, _, 4, 9, _, _, _, _

#const n = 10.

cell(1, 3, 1).
cell(1, 4, 5).
cell(1, 5, 2).
cell(1, 6, 6).
cell(1, 7, 7).
cell(1, 8, 8).
cell(2, 2, 1).
cell(2, 3, 5).
cell(2, 4, 2).
cell(2, 7, 6).
cell(2, 8, 7).
cell(2, 9, 8).
cell(3, 1, 1).
cell(3, 2, 5).
cell(3, 3, 2).
cell(3, 8, 6).
cell(3, 9, 7).
cell(3, 10, 8).
cell(4, 1, 5).
cell(4, 2, 2).
cell(4, 9, 6).
cell(4, 10, 7).
cell(5, 1, 2).
cell(5, 10, 6).
cell(6, 1, 4).
cell(6, 2, 10).
cell(6, 9, 3).
cell(6, 10, 9).
cell(7, 2, 4).
cell(7, 3, 10).
cell(7, 8, 3).
cell(7, 9, 9).
cell(8, 3, 4).
cell(8, 4, 10).
cell(8, 7, 3).
cell(8, 8, 9).
cell(9, 4, 4).
cell(9, 5, 10).
cell(9, 6, 3).
cell(9, 7, 9).
cell(10, 5, 4).
cell(10, 6, 9).
