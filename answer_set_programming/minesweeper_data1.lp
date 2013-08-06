% Problem from Gecode/examples/minesweeper.cc  problem 1
#const r = 8.
#const c = 8.

% _,2,_,2,1,1,_,_,
% _,_,4,_,2,_,_,2,
% 2,_,_,2,_,_,3,_,
% 2,_,2,2,_,3,_,3,
% _,_,1,_,_,_,4,_,
% 1,_,_,_,2,_,_,3,
% _,2,_,2,2,_,3,_,
% 1,_,1,_,_,1,_,1,

number(1, 2, 2).
number(1, 4, 2).
number(1, 5, 1).
number(1, 6, 1).
number(2, 3, 4).
number(2, 5, 2).
number(2, 8, 2).
number(3, 1, 2).
number(3, 4, 2).
number(3, 7, 3).
number(4, 1, 2).
number(4, 3, 2).
number(4, 4, 2).
number(4, 6, 3).
number(4, 8, 3).
number(5, 3, 1).
number(5, 7, 4).
number(6, 1, 1).
number(6, 5, 2).
number(6, 8, 3).
number(7, 2, 2).
number(7, 4, 2).
number(7, 5, 2).
number(7, 7, 3).
number(8, 1, 1).
number(8, 3, 1).
number(8, 6, 1).
number(8, 8, 1).
