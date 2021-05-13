%% From Prolog's example cyclic.pl
%% But here we try to invent path/3.

%% Positive examples

pos(cyclic(g1)).
pos(cyclic(g2)).
pos(cyclic(g3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Negative examples

neg(cyclic(g4)).
neg(cyclic(g5)).
