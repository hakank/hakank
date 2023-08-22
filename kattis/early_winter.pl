% https://open.kattis.com/problems/earlywinter
% 1s
% 1.6 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,Dm|Ns],Ss),
    N1 is N-1,
    findall(Y,(between(0,N1,Y),
                 nth0(Y,Ns,V),
                 V =< Dm
                ),
            Ys),
    (Ys == [] ->
        writeln("It had never snowed this early!")
    ;
        min_list(Ys,Max),
        format("It hadn't snowed this early in ~d years!~n",[Max])
    ).
