% https://open.kattis.com/problems/jumbojavelin
% 1s
% 1.4 Easy

% Shorter with read_string/3

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    length(Ns,Len),
    R is Sum - Len + 1,
    format('~d~n',[R]).
