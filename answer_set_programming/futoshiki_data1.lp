% Example from Tailor model futoshiki.param/futoshiki.param
% Futoshiki instance, by Andras Salamon
% specify the numbers in the grid
%

#const size = 5.

%
% Solution:
% 5 1 3 2 4
% 1 4 2 5 3
% 2 3 1 4 5
% 3 5 4 1 2
% 4 2 5 3 1
% 
%

% Hints
%   _, _, 3, 2, _,
%   _, _, _, _, _,
%   _, _, _, _, _,
%   _, _, _, _, _,
%   _, _, _, _, _
%
hint(1, 3, 3).
hint(1, 4, 2).

%
% The "less than" requirments
% 
% a,b, c,d mean cell(a,b) < cell(c,d)
%
less_than(1,2, 1,1).
less_than(1,4, 1,5).
less_than(2,3, 1,3).
less_than(3,3, 2,3).
less_than(3,4, 2,4).
less_than(2,5, 3,5).
less_than(3,2, 4,2).
less_than(4,4, 4,3).
less_than(5,2, 5,1).
less_than(5,4, 5,3).
less_than(5,5, 4,5).
