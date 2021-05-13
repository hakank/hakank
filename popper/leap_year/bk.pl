
% From https://github.com/michael-siebers/metagol/blob/ilp2018/examples/leapyear1.pl
%% background knowledge

% """
% This is equivalent to B_max described in the paper.
% Please note that possible dividers were computed as
% pre-processing step in the experiments.
% """
% hakank: These are the divisors of 400.
known_divider(1).
known_divider(2).
known_divider(4).
known_divider(5).
known_divider(8).
known_divider(10).
known_divider(16).
known_divider(20).
known_divider(25).
known_divider(40).
known_divider(50).
known_divider(80).
known_divider(100).
known_divider(200).
known_divider(400).

%% Hard code constants
known_divider1(1).
known_divider2(2).
known_divider4(4).
known_divider5(5).
known_divider8(8).
known_divider10(10).
known_divider16(16).
known_divider20(20).
known_divider25(25).
known_divider40(40).
known_divider50(50).
known_divider80(80).
known_divider100(100).
known_divider200(200).
known_divider400(400).

divisible(X,Y) :- 
    nonvar(X), 
    (nonvar(Y); known_divider(Y)),
    X mod Y =:= 0.

not_divisible(X,Y) :- 
    nonvar(X),
    (nonvar(Y); known_divider(Y)),
    X mod Y =\= 0.


eq(X,Y) :- X =:= Y.