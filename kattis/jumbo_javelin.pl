% https://open.kattis.com/problems/jumbojavelin
% 1s
% 1.4 Easy
main :-
    read_string(_),
    read_all_string(Ss),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    length(Ns,Len),
    R is Sum - Len + 1,
    format('~d~n',[R]).
main.

read_all_string(S) :-
    read_string(In),
    read_all_string(In,[],S).
read_all_string(end_of_file,S,S).
read_all_string(In,S0,[In|S]) :-
    read_string(S2),
    read_all_string(S2,S0,S).

read_string(S) :-
    read_line_to_string(user_input,S).

