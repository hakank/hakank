% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% A splitter,page 35
% 131072 solutions.
%
#const r=11.
#const c=11.

% _,_,_,0,_,_,_,0,_,_,_,
% _,_,_,0,1,_,1,0,_,_,_,
% _,_,_,0,1,_,1,0,_,_,_,
% 0,0,0,0,1,1,1,0,0,0,0,
% _,1,1,1,1,_,1,1,1,1,_,
% _,_,_,1,_,2,_,1,_,_,_,
% _,1,1,1,1,_,1,1,1,1,_,
% 0,0,0,0,1,1,1,0,0,0,0,
% _,_,_,0,1,_,1,0,_,_,_,
% _,_,_,0,1,_,1,0,_,_,_,
% _,_,_,0,_,_,_,0,_,_,_,

number(1, 4, 0).
number(1, 8, 0).
number(2, 4, 0).
number(2, 5, 1).
number(2, 7, 1).
number(2, 8, 0).
number(3, 4, 0).
number(3, 5, 1).
number(3, 7, 1).
number(3, 8, 0).
number(4, 1, 0).
number(4, 2, 0).
number(4, 3, 0).
number(4, 4, 0).
number(4, 5, 1).
number(4, 6, 1).
number(4, 7, 1).
number(4, 8, 0).
number(4, 9, 0).
number(4, 10, 0).
number(4, 11, 0).
number(5, 2, 1).
number(5, 3, 1).
number(5, 4, 1).
number(5, 5, 1).
number(5, 7, 1).
number(5, 8, 1).
number(5, 9, 1).
number(5, 10, 1).
number(6, 4, 1).
number(6, 6, 2).
number(6, 8, 1).
number(7, 2, 1).
number(7, 3, 1).
number(7, 4, 1).
number(7, 5, 1).
number(7, 7, 1).
number(7, 8, 1).
number(7, 9, 1).
number(7, 10, 1).
number(8, 1, 0).
number(8, 2, 0).
number(8, 3, 0).
number(8, 4, 0).
number(8, 5, 1).
number(8, 6, 1).
number(8, 7, 1).
number(8, 8, 0).
number(8, 9, 0).
number(8, 10, 0).
number(8, 11, 0).
number(9, 4, 0).
number(9, 5, 1).
number(9, 7, 1).
number(9, 8, 0).
number(10, 4, 0).
number(10, 5, 1).
number(10, 7, 1).
number(10, 8, 0).
number(11, 4, 0).
number(11, 8, 0).
