% https://open.kattis.com/problems/icpcawards
% 1s
% 1.6 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss,[],[],R),
    length(Rs,12),
    append(Rs,_,R),
    maplist(writeln,Rs).
main.

s([],_Us,S,S).
s([H|T],Us0,S0,S) :-
    split_string(H," ","",[U,_]),
    (member(U,Us0) ->
        S1 = S0,
        Us1 = Us0
    ;
        append(S0,[H],S1),
        append(Us0,[U],Us1)
    ),
    s(T,Us1,S1,S).
