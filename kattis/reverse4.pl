% https://open.kattis.com/problems/ofugsnuid
% 1s
% 1.3-1.5 Easy

% Shorter using read_string/2: 130 chars
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    reverse(Ss,R),
    maplist(writeln,R).
