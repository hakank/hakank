%% From Progol examples last.pl
% Last in list problem. Learns recursive clauses for last(Element,List)

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Positive examples

pos(last(a,[a])).
pos(last(b,[b])).
pos(last(c,[c])).
pos(last(a,[b,a])).
pos(last(b,[a,b])).
pos(last(c,[a,c])).
pos(last(c,[b,c])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Negative examples

neg(last(X,[])).
neg(last(X,[Y]), not(X==Y)).
neg(last(1,[1,2])).
neg(last(1,[2])).
neg(last(a,[c,a,b])).
neg(last(0,[1,2])).

