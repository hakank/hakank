% https://open.kattis.com/problems/nsum
% 1s
% 1.4 Easy
main :-
    rs(_),
    rs(S),
    split_string(S," ", "", Ss),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    format('~d~n',[Sum]).
main.

rs(S) :- read_line_to_string(user_input,S).