% https://open.kattis.com/problems/detaileddifferences
% 1s
% 1.4 Easy

main :-
    read_code(_),
    once(read_all_code(All)),
    check_pairs(All).

main.

check_pairs([]).
check_pairs([P1,P2|Rest]) :-
    format('~s~n~s~n',[P1,P2]),
    check_pair(P1,P2),
    nl,
    nl,
    check_pairs(Rest).

check_pair([],[]).
check_pair([A|As],[B|Bs]) :-
    (A == B -> T = '.' ; T = '*'),
    write(T),
    check_pair(As,Bs).

read_all_code(S) :-
    read_code(In),
    read_all_code(In,[],S).
read_all_code(end_of_file,S,S).
read_all_code(In,S0,[In|S]) :-
    read_code(S2),
    read_all_code(S2,S0,S).

read_code(S) :-
    read_line_to_codes(user_input,S).
