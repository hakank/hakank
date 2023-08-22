% https://open.kattis.com/problems/electricaloutlets
% Time limit: 1s
% Diff 1.3 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([S|Nss]) :-
    split_string(S," ","",Ss),
    maplist(number_string,Ns,Ss),
    check_circuit(Ns,Num),
    writeln(Num),
    s(Nss).
check_circuit(Ns, Num) :-
    sum_list(Ns,Sum),
    length(Ns,Len),
    Num is Sum - 2*(Len-2) - 2*1 + 1.
