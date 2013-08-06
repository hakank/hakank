% The graph from the Global Constraint catalog
% http://www.emn.fr/x-info/sdemasse/gccat/Cclique.html
% 
% 1 -> 2,4
% 2 -> 1,3,5
% 3 -> 2,5
% 4 -> 1,5
% 5 -> 2,3,4
%   
% The found clique (N=3) is {2,3,5}.
%
% % 1 2 3 4 5
%   1,1,0,1,0, % 1
%   1,1,1,0,1, % 2
%   0,1,1,0,1, % 3
%   1,0,0,1,1, % 4
%   0,1,1,1,1, % 5

e(1, 1).
e(1, 2).
e(1, 4).
e(2, 1).
e(2, 2).
e(2, 3).
e(2, 5).
e(3, 2).
e(3, 3).
e(3, 5).
e(4, 1).
e(4, 4).
e(4, 5).
e(5, 2).
e(5, 3).
e(5, 4).
e(5, 5).

#const n=2.