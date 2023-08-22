% https://open.kattis.com/problems/speeding
% 1s
% 1.6 Easy

main :-
    rs(_),ra(L),
    findall(Speed,(nextto([T1,V1],[T2,V2],L),Speed is floor((V2-V1)/(T2-T1))  ),Ps),
    max_list(Ps,Max), writeln(Max).
main.

ra(S) :-
    rs(In),
    ra(In,[],S).
ra(end_of_file,S,S).
ra(In,S0,[[T,V]|S]) :-
    split_string(In," ","",InS),
    maplist(number_string,[T,V],InS),
    rs(S2),
    ra(S2,S0,S).
rs(S) :-
    read_line_to_string(user_input,S).
