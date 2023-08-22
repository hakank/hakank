% https://open.kattis.com/problems/acm
% 1s
% 1.6 Easy

% Note: the penalty is only if the problem was finally solved.

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n", Ss),
    findall(Ts,(member(T,Ss),split_string(T," ","",Ts)),Tss),
    s(Ss,Tss,0,T,0,C),
    format('~d ~d~n',[C,T]).
main.

s([],_,T,T,C,C).
s(["-1"],_,T,T,C,C).
s([S|Ss],Tss,T0,T,C0,C) :-
    split_string(S," ","",[TimeS,Q,P]),
    number_string(Time,TimeS),   
    (P == "right" ->
        Score is Time,
        C1 is C0 + 1
    ;
        (member([_,Q,"right"],Tss) -> 
            Score is 20
        ;
            Score is 0
        ),
        C1 is C0
    ),
    T1 is T0 + Score,
    s(Ss,Tss,T1,T,C1,C).