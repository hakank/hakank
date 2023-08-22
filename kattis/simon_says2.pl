% https://open.kattis.com/problems/simonsays
% 1s
% 1.6 Easy

% Using read_string/3

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss). 
s([]).
s([S|T]):-(atom_concat("Simon says",X,S)->writeln(X);true),s(T).
