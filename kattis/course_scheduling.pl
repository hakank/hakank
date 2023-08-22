% https://open.kattis.com/problems/coursescheduling
% 1s
% 1.8 Easy

main :-
    read_string(user_input,10000000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss,[],Ls),
    sort(Ls,Sorted),
    findall(C,member([C,_,_],Sorted),Cs),
    clumped(Cs,Cl),
    p(Cl).
p([]).
p([C-Len|Cs]) :-
    format('~s ~d~n',[C,Len]),
    p(Cs).

s([],S,S).
s([L|Ls],S0,[[C,F,Ln]|S]) :-
    split_string(L," ","",[F,Ln,C]),
    s(Ls,S0,S).
    