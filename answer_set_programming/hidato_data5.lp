% Problem from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

#const n=6.
% problem 3 (Beginner)
% 
%  0,26,0,0,0,18,
%  0,0,27,0,0,19,
%  31,23,0,0,14,0,
%  0,33,8,0,15,1,
%  0,0,0,5,0,0,
%  35,36,0,10,0,0

hint(1, 2, 26).
hint(1, 6, 18).
hint(2, 3, 27).
hint(2, 6, 19).
hint(3, 1, 31).
hint(3, 2, 23).
hint(3, 5, 14).
hint(4, 2, 33).
hint(4, 3, 8).
hint(4, 5, 15).
hint(4, 6, 1).
hint(5, 4, 5).
hint(6, 1, 35).
hint(6, 2, 36).
hint(6, 4, 10).
