% Problem from Gecode/examples/minesweeper.cc  problem 9
%       2...2...2
%       .4.4.3.4.
%       ..4...1..
%       .4.3.3.4.
%       2.......2
%       .5.4.5.4.
%       ..3...3..
%       .4.3.5.6.
%       2...1...2
#const r = 9.
#const c = 9.

% 2,_,_,_,2,_,_,_,2,
% _,4,_,4,_,3,_,4,_,
% _,_,4,_,_,_,1,_,_,
% _,4,_,3,_,3,_,4,_,
% 2,_,_,_,_,_,_,_,2,
% _,5,_,4,_,5,_,4,_,
% _,_,3,_,_,_,3,_,_,
% _,4,_,3,_,5,_,6,_,
% 2,_,_,_,1,_,_,_,2,
number(1, 1, 2).
number(1, 5, 2).
number(1, 9, 2).
number(2, 2, 4).
number(2, 4, 4).
number(2, 6, 3).
number(2, 8, 4).
number(3, 3, 4).
number(3, 7, 1).
number(4, 2, 4).
number(4, 4, 3).
number(4, 6, 3).
number(4, 8, 4).
number(5, 1, 2).
number(5, 9, 2).
number(6, 2, 5).
number(6, 4, 4).
number(6, 6, 5).
number(6, 8, 4).
number(7, 3, 3).
number(7, 7, 3).
number(8, 2, 4).
number(8, 4, 3).
number(8, 6, 5).
number(8, 8, 6).
number(9, 1, 2).
number(9, 5, 1).
number(9, 9, 2).
