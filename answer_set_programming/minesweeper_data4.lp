% Problem from Gecode/examples/minesweeper.cc  problem 4
#const r = 10.
#const c = 10.

% 0,_,0,_,1,_,_,1,1,_,
% 1,_,2,_,2,_,2,2,_,_,
% _,_,_,_,_,_,2,_,_,2,
% _,2,3,_,1,1,_,_,_,_,
% 0,_,_,_,_,_,_,2,_,1,
% _,_,_,2,2,_,1,_,_,_,
% _,_,_,_,_,3,_,3,2,_,
% _,5,_,2,_,_,_,3,_,1,
% _,3,_,1,_,_,3,_,_,_,
% _,2,_,_,_,1,2,_,_,0,

number(1, 1, 0).
number(1, 3, 0).
number(1, 5, 1).
number(1, 8, 1).
number(1, 9, 1).
number(2, 1, 1).
number(2, 3, 2).
number(2, 5, 2).
number(2, 7, 2).
number(2, 8, 2).
number(3, 7, 2).
number(3, 10, 2).
number(4, 2, 2).
number(4, 3, 3).
number(4, 5, 1).
number(4, 6, 1).
number(5, 1, 0).
number(5, 8, 2).
number(5, 10, 1).
number(6, 4, 2).
number(6, 5, 2).
number(6, 7, 1).
number(7, 6, 3).
number(7, 8, 3).
number(7, 9, 2).
number(8, 2, 5).
number(8, 4, 2).
number(8, 8, 3).
number(8, 10, 1).
number(9, 2, 3).
number(9, 4, 1).
number(9, 7, 3).
number(10, 2, 2).
number(10, 6, 1).
number(10, 7, 2).
number(10, 10, 0).
