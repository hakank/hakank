% https://open.kattis.com/problems/pot
% 1s
% 1.4 Easy

% Using read_string/3 instead.

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns,0,R),
    format('~d~n',[R]).
s([],R,R).
s([N|Ns],S0,S) :-
    S1 is S0+(N div 10)^(N mod 10),
    s(Ns,S1,S).
