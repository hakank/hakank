% https://open.kattis.com/problems/vote
% 1s
% 2.0 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns).

s([]).
s([N|Ss]) :-
    length(A,N),
    append(A,Rest,Ss),
    max_list(A,M),
    sum_list(A,Sum),
    findall(P,(between(1,N,P),nth1(P,A,M)),Ps),
    length(Ps,Len),
    (Len =:= 1 -> (M > Sum / 2 -> T = "majority" ; T = "minority" ),  format("~s winner ~d~n",[T,Ps]) ; writeln("no winner")),
    s(Rest).