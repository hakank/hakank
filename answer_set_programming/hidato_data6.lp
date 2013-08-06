% Problem from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

% Problem 15 (Intermediate)
%   64, 0, 0, 0, 0, 0, 0, 0,
%    1,63, 0,59,15,57,53, 0,
%    0, 4, 0,14, 0, 0, 0, 0,
%    3, 0,11, 0,20,19, 0,50,
%    0, 0, 0, 0,22, 0,48,40,
%    9, 0, 0,32,23, 0, 0,41,
%   27, 0, 0, 0,36, 0,46, 0,
%   28,30, 0,35, 0, 0, 0, 0,
%
#const n = 8.
hint(1, 1, 64).
hint(2, 1, 1).
hint(2, 2, 63).
hint(2, 4, 59).
hint(2, 5, 15).
hint(2, 6, 57).
hint(2, 7, 53).
hint(3, 2, 4).
hint(3, 4, 14).
hint(4, 1, 3).
hint(4, 3, 11).
hint(4, 5, 20).
hint(4, 6, 19).
hint(4, 8, 50).
hint(5, 5, 22).
hint(5, 7, 48).
hint(5, 8, 40).
hint(6, 1, 9).
hint(6, 4, 32).
hint(6, 5, 23).
hint(6, 8, 41).
hint(7, 1, 27).
hint(7, 5, 36).
hint(7, 7, 46).
hint(8, 1, 28).
hint(8, 2, 30).
hint(8, 4, 35).
