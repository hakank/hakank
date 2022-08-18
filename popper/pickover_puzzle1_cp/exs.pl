%% Pickover puzzle: https://twitter.com/pickover/status/1504602497280786435
%%  What number should replaced the question mark?
%%     3         9       8
%%    44        32      75
%%  8    4    7   2    7  ?
%%
%% Encoded as
%%    a
%%    d 
%%   b  c
%% a op b op c == d
%%
%% The solution:
%% c = d / (a + b)
%% i.e.
%% d = (a+b)*c
%%
%% https://gpuzzles.com/mind-teasers/very-easy-number-sequence-puzzle/
%% Also see genetic_programming/jgap/pickover_puzzle1.conf
%%
%% 

% Placing unknown (c) last
pos(f(3,8,44,4)).
pos(f(9,7,32,2)).
% pos(f(8,7,75,X)). % What is X?


neg(f(9,7,32,3)).
neg(f(9,7,33,2)).