% 
% Place number puzzle in ASP.
%
% http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
% """
% Place numbers 1 through 8 on nodes
% - each number appears exactly once
% - no connected nodes have consecutive numbers
%      2 - 5 
%    / | X | \
%  1 - 3 - 6 - 8
%    \ | X | /
%      4 - 7
% """
% 
% Two solutions (plus their reverses):
% x = [2, 5, 8, 6, 3, 1, 4, 7]
% 
% x = [2, 6, 8, 5, 4, 1, 3, 7]
% 
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const n = 8.

% the graph
g(1, 2;3;4).
g(2, 1;3;5;6).
g(3, 2;4;6;7).
g(4, 1;3;6;7).
g(5, 2;3;6;8).
g(6, 2;3;4;5;7;8).
g(7, 3;4;6;8).
g(8, 5;6;7).

% domain
number(1..n).

% alldifferent 
1 { x(I, N) : number(N) } 1 :- number(I).
1 { x(I, N) : number(I) } 1 :- number(N).

% ensure that two adjacent number is no consecutive
:- x(I1,N1), x(I2,N2), g(I1,I2), #abs(N1-N2) <= 1.

% symmetry breaking
:-  x(1,N1), x(n, N2), N1 >= N2.

#hide.
#show x(I,N).
