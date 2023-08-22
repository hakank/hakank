% https://open.kattis.com/problems/scalingrecipe
% 1s
% 1.9 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_,X,Y|Ns],Ss),
    s(Ns,X,Y).
s([],_,_).
s([N|Ns],X,Y) :-
    T is N*Y div X,
    writeln(T),
    s(Ns,X,Y).