% https://open.kattis.com/problems/everywhere
% 1s
% 1.4 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([N0|Ss]) :-
    number_string(N,N0),
    length(L,N),
    append(L,Rest,Ss),
    sort(L,Sorted),
    length(Sorted,Len),
    writeln(Len),
    s(Rest).
