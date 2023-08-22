% https://open.kattis.com/problems/babybites
% 1s
% 1.8 Easy

% 318 chars
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    ( s(1,Ss) -> 
        T="makes sense"
    ;
        T="something is fishy"
    ),
    writeln(T).
s(_,[]).
s(I,[S|Ss]) :-
    (S == "mumble"
    ;
        number_string(N,S),
        N =:= I
    ),
    I1 is I+1,
    s(I1,Ss).

/*
% Compressed: 218 chars
main:-read_string(user_input,1000000,S),split_string(S,"\n ","\n ",[_|Ss]),(s(1,Ss)->T="makes sense";T="something is fishy"),writeln(T).
s(_,[]). s(I,[S|Ss]):-(S=="mumble";number_string(N,S),N=:=I),I1 is I+1,s(I1,Ss).
*/


    