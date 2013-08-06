% Rogo problem:
% Bigger picture of the problem instance
% http://mat.tepper.cmu.edu/blog/wp-content/uploads/2010/12/IntroRogo1.jpg
%
% Problem: W: white, B: black, Number: point
%
% 2, W, W, W, W, W, W, W, W,
% W, 3, W, W, 1, W, W, 2, W,
% W, W, W, W, W, W, B, W, 2,
% W, W, 2, B, W, W, W, W, W,
% W, W, W, W, 2, W, W, 1, W,
%
% The solution should be (some like) this path with 
% the (optimal) value of 8 points.
%
%  2,2   3 points
%  2,3
%  2,4
%  2,5   1 point
%  3,5
%  4,5
%  5,5   2 points
%  5,4
%  5,3
%  4,3   2 points
%  4,2
%  3,2
%  (back to 2,2) 

% Points taken
%  2,2
%  2,5
%  5,5
%  4,3

#const max_steps = 12.
#const r = 5.
#const c = 9.

% the point cells
point(1,1,2).
point(2,2,3).
point(2,5,1).
point(2,8,2).
point(3,9,3).
point(4,3,2).
point(5,5,2).
point(5,8,1).

% black cells (to avoid)
black(3,7).
black(4,4).


