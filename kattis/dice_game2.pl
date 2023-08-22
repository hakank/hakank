% https://open.kattis.com/problems/dicegame
% 1s
% 1.6 Easy

% Uncompressed 324 chars
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[A,B,C,D,E,F,G,H],Ss),    
    X is ((B+D)-(F+H))+((A+C)-(E+G)),        
    (X =:= 0 ->
        T="Tie"
    ;
        (X > 0 ->
            T="Gunnar"
        ;
            T="Emma"
        )
    ),
    writeln(T).

/*
% compressed: 203 chars
main:-read_string(user_input,100000,S),split_string(S,"\n ","\n ",Ss),maplist(number_string,[A,B,C,D,E,F,G,H],Ss),X is ((B+D)-(F+H))+((A+C)-(E+G)),(X=:=0->T="Tie";(X>0->T="Gunnar";T="Emma")),writeln(T).
*/
