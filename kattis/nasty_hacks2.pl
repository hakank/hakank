% https://open.kattis.com/problems/nastyhacks
% 1s
% 1.4 Easy

% Shorter with read_string/3

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
main.
s([]).
s([S|Ss]) :-
    split_string(S, " ", "", Ts),
    maplist(number_string,[NotA,AProfit,ACost],Ts),
    Ad is AProfit - ACost,
    ( NotA =:= Ad -> T="does not matter" ; (NotA > Ad -> T="do not advertise" ; T="advertise" )),
    writeln(T),
    s(Ss).
