% Problem from Gecode/examples/minesweeper.cc  problem 6
#const r = 8.
#const c = 8.

% _,3,2,_,_,1,_,_,
% _,_,_,_,1,_,_,3,
% 3,_,_,2,_,_,_,4,
% _,5,_,_,_,5,_,_,
% _,_,6,_,_,_,5,_,
% 3,_,_,_,5,_,_,4,
% 2,_,_,5,_,_,_,_,
% _,_,2,_,_,3,4,_,


number(1, 2, 3).
number(1, 3, 2).
number(1, 6, 1).
number(2, 5, 1).
number(2, 8, 3).
number(3, 1, 3).
number(3, 4, 2).
number(3, 8, 4).
number(4, 2, 5).
number(4, 6, 5).
number(5, 3, 6).
number(5, 7, 5).
number(6, 1, 3).
number(6, 5, 5).
number(6, 8, 4).
number(7, 1, 2).
number(7, 4, 5).
number(8, 3, 2).
number(8, 6, 3).
number(8, 7, 4).
