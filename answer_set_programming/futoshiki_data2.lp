% Example from http://en.wikipedia.org/wiki/Futoshiki
%
% Solution:
%
% 5 4 3 2 1
% 4 3 1 5 2
% 2 1 4 3 5
% 3 5 2 1 4
% 1 2 5 4 3

#const size = 5.

%
% Hints:
% _, _, _, _, _,
% 4, _, _, _, 2,
% _, _, 4, _, _,
% _, _, _, _, 4,
% _, _, _, _, _
%
hint(2, 1, 4).
hint(2, 5, 2).
hint(3, 3, 4).
hint(4, 5, 4).

%
% Less than hints
%
less_than(1,2, 1,1).
less_than(1,4, 1,3).
less_than(1,5, 1,4).
less_than(4,4, 4,5).
less_than(5,1, 5,2).
less_than(5,2, 5,3).

