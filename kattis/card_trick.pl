% https://open.kattis.com/problems/cardtrick2
% 1s
% 1.8 Easy

% Generated all N=1..13 by the Picat program card_trick.pi
% Perhaps this is considered cheating...

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns).

s([]).
s([N|Ns]) :-
    p(N,P),
    maplist(format('~d '),P),
    nl,
    s(Ns).

p(1,[1]).
p(2,[2,1]).
p(3,[3,1,2]).
p(4,[2,1,4,3]).
p(5,[3,1,4,5,2]).
p(6,[4,1,6,3,2,5]).
p(7,[5,1,3,4,2,6,7]).
p(8,[3,1,7,5,2,6,8,4]).
p(9,[7,1,8,6,2,9,4,5,3]).
p(10,[9,1,8,5,2,4,7,6,3,10]).
p(11,[5,1,6,4,2,10,11,7,3,8,9]).
p(12,[7,1,4,9,2,11,10,8,3,6,5,12]).
p(13,[4,1,13,11,2,10,6,7,3,5,12,9,8]).
