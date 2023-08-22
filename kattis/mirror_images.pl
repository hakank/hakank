% https://open.kattis.com/problems/mirror
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).

s(_,[]).
s(I,[S|Ss]) :-
    format("Test ~d~n",[I]),
    split_string(S," ","",RC),
    maplist(number_string,[R,_],RC),
    length(M,R),
    append(M,Rest,Ss),
    maplist(string_chars,M,Mc),
    reverse(Mc,Rev),
    maplist(reverse,Rev,T),
    w(T),
    I1 is I+1,
    s(I1,Rest).

w([]).
w([L|Ls]) :-
    format("~s",[L]),
    nl,
    w(Ls).
            