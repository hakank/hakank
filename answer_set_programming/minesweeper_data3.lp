% Problem from Gecode/examples/minesweeper.cc  problem 3
#const r = 8.
#const c = 8.

% 2,_,_,_,3,_,1,_,
% _,5,_,4,_,_,_,1,
% _,_,5,_,_,4,_,_,
% 2,_,_,_,4,_,5,_,
% _,2,_,4,_,_,_,2,
% _,_,5,_,_,4,_,_,
% 2,_,_,_,5,_,4,_,
% _,3,_,3,_,_,_,2,

number(1, 1, 2).
number(1, 5, 3).
number(1, 7, 1).
number(2, 2, 5).
number(2, 4, 4).
number(2, 8, 1).
number(3, 3, 5).
number(3, 6, 4).
number(4, 1, 2).
number(4, 5, 4).
number(4, 7, 5).
number(5, 2, 2).
number(5, 4, 4).
number(5, 8, 2).
number(6, 3, 5).
number(6, 6, 4).
number(7, 1, 2).
number(7, 5, 5).
number(7, 7, 4).
number(8, 2, 3).
number(8, 4, 3).
number(8, 8, 2).
