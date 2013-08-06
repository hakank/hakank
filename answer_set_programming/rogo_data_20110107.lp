% From the problem per 2011-01-07:
%  max step: 16
%  good: 34 points
%  best: 36 points
%
%  %1 2 3 4 5 6 7
%   4,7,W,W,W,W,3, % 1
%   W,W,W,W,3,W,4, % 2
%   W,W,4,W,7,W,W, % 3
%   7,W,3,W,W,W,W, % 4
%   B,B,B,W,3,W,W, % 5
%   B,B,W,7,W,W,7, % 6
%   B,B,W,W,W,4,B, % 7
%   B,4,4,W,W,W,B, % 8
%   B,W,W,W,W,3,B, % 9
%   W,W,3,W,4,B,B, % 0
%   3,W,W,W,W,B,B, % 1
%   7,W,7,4,B,B,B  % 2
%
% Unfortunately there is no direct link 
% to this problem (it's just cumulative of last few days.)
% http://www.rogopuzzle.co.nz/paper-rogos/
%
% Solution (from the site): 36 points
% path(1,8,2).
% path(2,8,3).
% path(3,9,3).
% path(4,10,3).
% path(5,10,4).
% path(6,10,5).
% path(7,11,5).
% path(8,11,4).
% path(9,12,4).
% path(10,12,3).
% path(11,12,2).
% path(12,12,1).
% path(13,11,1).
% path(14,11,2).
% path(15,10,2).
% path(16,9,2).
% 
%
% The path with 36 points is found almost
% immediately (~1 second), then it takes long time 
% to prove it correct.
%
% path:
% 1,11,1
% 2,11,2
% 3,10,2
% 4,9,2
% 5,8,2
% 6,8,3
% 7,9,3
% 8,10,3
% 9,10,4
% 10,10,5
% 11,11,5
% 12,11,4
% 13,12,4
% 14,12,3
% 15,12,2
% 16,12,1

% Got these points:
% 8,2,4
% 8,3,4
% 10,3,3
% 10,5,4
% 11,1,3
% 12,1,7
% 12,3,7
% 12,4,4

#const max_steps = 16.
#const r=12.
#const c=7.

point(1,1,4).
point(1,2,7).
point(1,7,3).
point(2,5,3).
point(2,7,4).
point(3,3,4).
point(3,5,7).
point(4,1,7).
point(4,3,3).
point(5,5,3).
point(6,4,7).
point(6,7,7).
point(7,6,4).
point(8,2,4).
point(8,3,4).
point(9,6,3).
point(10,3,3).
point(10,5,4).
point(11,1,3).
point(12,1,7).
point(12,3,7).
point(12,4,4).

black(5, 1;2;3).
black(6, 1;2).
black(7, 1;2;7).
black(8, 1;7).
black(9, 1;7).
black(10, 6;7).
black(11, 6;7).
black(12, 5;6;7).

