
wlist([]).
wlist([W|Rest]) :- constant(W), wlist(Rest).

%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge

np(S1,S2) :- det(S1,S3), noun(S3,S2).

det([a|S],S).
det([the|S],S).

vp(S1,S2) :- verb(S1,S2).
vp(S1,S2) :- verb(S1,S3), prep(S3,S2).

noun([man|S],S).
noun([dog|S],S).
noun([house|S],S).
noun([ball|S],S).

verb([takes|S],S).
verb([walks|S],S).
verb([hits|S],S).

prep([at|S],S).
prep([to|S],S).
prep([on|S],S).
prep([in|S],S).
