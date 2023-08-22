% https://open.kattis.com/problems/coffeecupcombo
% 1s
% 1.6 Easy

% Wrong answer on test 7/18.
% Fixed: The problem was that the buffer (100000) was too small...
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_,L]),
    string_codes(L,Ls),
    s(Ls,0,0,Res),
    writeln(Res).
main.

s([],_,S,S).
s([H|T],C0,S0,S) :-
    writeln(s([H|T],C0,S0,S)),
    (H =:= 49 ->
        C1 is min(2,C0+2),
        S1 is S0 + 1
    ;
        (C0 > 0 ->
            C1 is C0 - 1,
            S1 is S0 + 1
        ;
            C1 is C0,
            S1 is S0 
        )
    ),
    s(T,C1,S1,S).