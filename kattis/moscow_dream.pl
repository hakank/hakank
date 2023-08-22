% https://open.kattis.com/problems/moscowdream
% 1s
% 2.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[A,B,C,N],Ss),
    ABC is A+B+C,
    ((A >= 1, B >= 1, C >= 1, A+B+C >= N, N>= 3) ->
        T="YES"
    ;
        T="NO"
    ),
    writeln(T).
