% Puzzle 2 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
#const n = 10.

% 0,_,_,_,_,_,3,4,_,3,
% _,_,_,4,_,_,_,7,_,_,
% _,_,5,_,2,2,_,4,_,3,
% 4,_,6,6,_,2,_,_,_,_,
% _,_,_,_,3,3,_,_,3,_,
% _,_,8,_,_,4,_,_,_,_,
% _,9,_,7,_,_,_,_,5,_,
% _,_,_,7,5,_,_,3,3,0,
% _,_,_,_,_,_,_,_,_,_,
% 4,4,_,_,2,3,3,4,3,_

% Note: the count is how many numbers surrounds this 
%       cell including the cell.
hint(1, 1, 0).
hint(1, 7, 3).
hint(1, 8, 4).
hint(1, 10, 3).
hint(2, 4, 4).
hint(2, 8, 7).
hint(3, 3, 5).
hint(3, 5, 2).
hint(3, 6, 2).
hint(3, 8, 4).
hint(3, 10, 3).
hint(4, 1, 4).
hint(4, 3, 6).
hint(4, 4, 6).
hint(4, 6, 2).
hint(5, 5, 3).
hint(5, 6, 3).
hint(5, 9, 3).
hint(6, 3, 8).
hint(6, 6, 4).
hint(7, 2, 9).
hint(7, 4, 7).
hint(7, 9, 5).
hint(8, 4, 7).
hint(8, 5, 5).
hint(8, 8, 3).
hint(8, 9, 3).
hint(8, 10, 0).
hint(10, 1, 4).
hint(10, 2, 4).
hint(10, 5, 2).
hint(10, 6, 3).
hint(10, 7, 3).
hint(10, 8, 4).
hint(10, 9, 3).
