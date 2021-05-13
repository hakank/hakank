%% From progol/examples/grammar.pl

%%%%%%%%%%%%%%%%%%%%
% Positive examples

pos(s([the,man,walks,the,dog],[])).
pos(s([the,dog,walks,to,the,man],[])).
pos(s([a,dog,hits,a,ball],[])).
pos(s([the,man,walks,in,the,house],[])).
pos(s([the,man,hits,the,dog],[])).
pos(s([a,ball,hits,the,dog],[])).

% See grammar2 for this example
% More complex positive examples.
%
% pos(s([a,man,hits,the,ball,at,the,dog],[])).
% pos(s([the,man,hits,the,ball,at,the,house],[])).
% pos(s([the,man,takes,the,dog,to,the,ball],[])).
% pos(s([a,man,takes,the,ball,to,the,house],[])).
% pos(s([the,dog,takes,the,ball,to,the,house],[])).
% pos(s([the,dog,takes,the,ball,to,the,man],[])).
% pos(s([the,man,hits,the,ball,to,the,dog],[])).
% pos(s([the,man,walks,the,dog,to,the,house],[])).

%%%%%%%%%%%%%%%%%%%%
% Negative examples

neg(s([a,dog,walks,the],[])).
neg(s([a,man,walks,the],[])).
neg(s([a,man,walks,the,walks],[])).
neg(s([a,man,walks,the,house,a],[])).
neg(s([a,man,walks,the,dog,at],[])).
neg(s([the,man,walks,the,dog,to,the],[])).
neg(s([the,dog],[])).
