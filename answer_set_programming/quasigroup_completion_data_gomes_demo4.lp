% Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
% (n = 10)
% Pattern #4. 
%  dark red   = 1
%  light blue = 2
%  dark blue  = 3
%  light red  = 4
% Note: There are no solutions to this problem.
% 1,_,_,_,_,_,_,_,_,_,
% 2,1,_,_,_,_,_,_,_,4,
% 3,2,1,_,_,_,_,_,4,_,
% _,3,2,1,_,_,_,4,_,_,
% _,_,3,2,1,_,4,_,_,_,
% _,_,_,3,2,1,_,_,_,_,
% _,_,_,_,3,2,1,_,_,_,
% _,_,_,4,_,3,2,1,_,_,
% _,_,4,_,_,_,3,2,1,_,
% _,4,_,_,_,_,_,3,2,1,

#const n = 10.


cell(1, 1, 1).
cell(2, 1, 2).
cell(2, 2, 1).
cell(2, 10, 4).
cell(3, 1, 3).
cell(3, 2, 2).
cell(3, 3, 1).
cell(3, 9, 4).
cell(4, 2, 3).
cell(4, 3, 2).
cell(4, 4, 1).
cell(4, 8, 4).
cell(5, 3, 3).
cell(5, 4, 2).
cell(5, 5, 1).
cell(5, 7, 4).
cell(6, 4, 3).
cell(6, 5, 2).
cell(6, 6, 1).
cell(7, 5, 3).
cell(7, 6, 2).
cell(7, 7, 1).
cell(8, 4, 4).
cell(8, 6, 3).
cell(8, 7, 2).
cell(8, 8, 1).
cell(9, 3, 4).
cell(9, 7, 3).
cell(9, 8, 2).
cell(9, 9, 1).
cell(10, 2, 4).
cell(10, 8, 3).
cell(10, 9, 2).
cell(10, 10, 1).
