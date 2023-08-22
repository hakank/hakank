% https://open.kattis.com/problems/speedlimit
% Time limit: 1s
% Diff 1.5 Easy

% A better (and shorter) DCG version using phrase_from_stream(p([W,L]),user_input) instead
:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(read_all(G0),user_input),
    delete(G0,[],G),
    check_groups(G).
main.

diffs(L,Diffs) :-
    append([0],L,L2),
    findall(Diff,(nextto(X,Y,L2), Diff is Y-X),Diffs).

check_groups([]).
check_groups([Group|Groups]) :-
    pairs_keys_values(Group, Speeds,Times),
    diffs(Times,Diffs),
    maplist(mult,Diffs,Speeds,Tot0),
    sum_list(Tot0,Tot),
    format('~d miles~n',[Tot]),
    check_groups(Groups).

mult(A,B,C) :- C is A*B.

read_group([Speed-Time|Groups]) --> integer(Speed), " ", integer(Time), "\n", read_group(Groups).
read_group([Speed-Time])        --> integer(Speed), " ", integer(Time).
read_group([])                  --> [].

read_all([Group|Groups]) --> integer(_), "\n", read_group(Group), "\n", read_all(Groups).
read_all([Group])        --> integer(_), "\n", read_group(Group).
