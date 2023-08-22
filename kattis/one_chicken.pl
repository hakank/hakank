% https://open.kattis.com/problems/onechicken
% 1s
% 1.9 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[N,M],Ss),
    D is abs(N-M),
    (D =:= 1 -> P = "";P = "s"),
    (N < M ->
        format("Dr. Chaz will have ~d piece~s of chicken left over!~n",[D,P])
    ;
        format("Dr. Chaz needs ~d more piece~s of chicken!~n",[D,P])
    ).
