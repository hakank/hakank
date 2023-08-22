% https://open.kattis.com/problems/judgingmoose
% 1s
% 1.5 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[L,R],Ss),
    ( (L=:=0,R=:=0) ->
        writeln("Not a moose")
    ;
        (L =:= R ->
            X is L+R,
            T = "Even"
        ;
            X is max(L,R)*2,
            T = "Odd"
        ),
        format('~s ~d~n',[T,X])
    ).
