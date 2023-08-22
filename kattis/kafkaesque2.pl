% https://open.kattis.com/problems/kafkaesque
% 1s
% 2.0 Easy

% Shorter: 198 chars
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    findall(A,(nextto(A,B,Ns),A>B),As),
    length(As,N),
    format("~d~n",[N+1]).
