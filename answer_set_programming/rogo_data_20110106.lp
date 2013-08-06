% 
% Rogo problem from 2011-01-06
% http://www.rogopuzzle.co.nz/paper-rogos/
% 16 steps, good: 28, best: 31
%
% B,W,6,W,W,3,B,
% 2,W,3,W,W,6,W,
% 6,W,W,2,W,W,2,
% W,3,W,W,B,B,B,
% W,W,W,2,W,2,B,
% W,W,W,3,W,W,W,
% 6,W,6,B,W,W,3,
% 3,W,W,W,W,W,6,
% B,2,W,6,W,2,B,
%
% Solution found:
%
% Path:
% 1,3,1
% 2,3,2
% 3,4,2
% 4,4,3
% 5,3,3
% 6,3,4
% 7,2,4
% 8,2,5
% 9,2,6
% 10,1,6
% 11,1,5
% 12,1,4
% 13,1,3
% 14,2,3
% 15,2,2
% 16,2,1
%
% Points:
% 1,3,6
% 1,6,3
% 2,1,2
% 2,3,3
% 2,6,6
% 3,1,6
% 3,4,2
% 4,2,3

#const max_steps = 16.
#const r=9.
#const c=7.
point(1, 3, 6).
point(1, 6, 3).
point(2, 1, 2).
point(2, 3, 3).
point(2, 6, 6).
point(3, 1, 6).
point(3, 4, 2).
point(3, 7, 2).
point(4, 2, 3).
point(5, 4, 2).
point(5, 6, 2).
point(6, 4, 3).
point(7, 1, 6).
point(7, 3, 6).
point(7, 7, 3).
point(8, 1, 3).
point(8, 7, 6).
point(9, 2, 2).
point(9, 4, 6).
point(9, 6, 2).

black(1, 1).
black(1, 7).
black(4, 5).
black(4, 6).
black(4, 7).
black(5, 7).
black(7, 4).
black(9, 1).
black(9, 7).
