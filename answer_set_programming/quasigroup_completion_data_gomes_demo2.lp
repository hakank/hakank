% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #2. 
% There are _many_ solutions to this problem.
% [
%   _,_,1,2,3,4,_,_,_,_,
%   _,1,2,3,_,_,4,_,_,_,
%   1,2,3,_,_,_,_,4,_,_,
%   2,3,_,_,_,_,_,_,4,_,
%   3,_,_,_,_,_,_,_,_,4,
%   5,6,_,_,_,_,_,_,_,_,
%   _,5,6,_,_,_,_,_,_,_,
%   _,_,5,6,_,_,_,_,_,_,
%   _,_,_,5,6,_,_,_,_,_,
%   _,_,_,_,5,6,_,_,_,_,

#const n = 10.

cell(1, 3, 1).
cell(1, 4, 2).
cell(1, 5, 3).
cell(1, 6, 4).
cell(2, 2, 1).
cell(2, 3, 2).
cell(2, 4, 3).
cell(2, 7, 4).
cell(3, 1, 1).
cell(3, 2, 2).
cell(3, 3, 3).
cell(3, 8, 4).
cell(4, 1, 2).
cell(4, 2, 3).
cell(4, 9, 4).
cell(5, 1, 3).
cell(5, 10, 4).
cell(6, 1, 5).
cell(6, 2, 6).
cell(7, 2, 5).
cell(7, 3, 6).
cell(8, 3, 5).
cell(8, 4, 6).
cell(9, 4, 5).
cell(9, 5, 6).
cell(10, 5, 5).
cell(10, 6, 6).
