% Problem https://open.kattis.com/problems/different
% 1s
% 2.6 Easy

main:-
    read_string(user_input,1000000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s([H|T]):-
    split_string(H," ","",X),
    maplist(number_string,[A,B],X),
    S is abs(A-B),
    writeln(S),
    s(T).

