%
% Puzzle 1 from 
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% 
#const n = 10.

% 1 2 3 4 5 6 7 8 9 0
% _,_,_,_,_,_,_,_,0,_, % 1
% _,8,8,_,2,_,0,_,_,_, % 2
% 5,_,8,_,_,_,_,_,_,_, % 3
% _,_,_,_,_,2,_,_,_,2, % 4
% 1,_,_,_,4,5,6,_,_,_, % 5
% _,0,_,_,_,7,9,_,_,6, % 6
% _,_,_,6,_,_,9,_,_,6, % 7
% _,_,6,6,8,7,8,7,_,5, % 8
% _,4,_,6,6,6,_,6,_,4, % 9
% _,_,_,_,_,_,3,_,_,_  %10

% Note: the count is how many numbers surrounds this 
%       cell including the cell.
hint(1,9,0).
hint(2,2,8).
hint(2,3,8).
hint(2,5,2).
hint(2,7,0).
hint(3,1,5).
hint(3,3,8).
hint(4,6,2).
hint(4,10,2).
hint(5,1,1).
hint(5,5,4).
hint(5,6,5).
hint(5,7,6).
hint(6,2,0).
hint(6,6,7).
hint(6,7,9).
hint(6,10,6).
hint(7,4,6).
hint(7,7,9).
hint(7,10,6).
hint(8,3,6).
hint(8,4,6).
hint(8,5,8).
hint(8,6,7).
hint(8,7,8).
hint(8,8,7).
hint(8,10,5).
hint(9,2,4).
hint(9,4,6).
hint(9,5,6).
hint(9,6,6).
hint(9,8,6).
hint(9,10,4).
hint(10,7,3).
