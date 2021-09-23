%% Facebook puzzle
% 1 + 4 = 5
% 2 + 5 = 12
% 3 + 6 = 21
% 5 + 8 = ??

%% f(A,B,C,D,E,F).
%% Expected solution:
%%   plus(C,D,G),plus(E,G,F).
%%
pos(f( 0, 0, 0, 1, 4, 5)).
pos(f( 1, 4, 5, 2, 5,12)).
pos(f( 2, 5,12, 3, 6,21)).

neg(f( 2, 5,12, 3, 6,7)).
