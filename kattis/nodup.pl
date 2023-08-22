% https://open.kattis.com/problems/nodup
% 1s
% 1.5 Easy

main :-
    readln(S),
    sort(S,R),
    (same_length(S,R) -> T = yes ; T = no),
    writeln(T).

/*
% Compressed: 68 chars
main:-readln(S),sort(S,R),(same_length(S,R)->T=yes;T=no),writeln(T).
*/

/*
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    sort(Ss,Sd),
    (same_length(Ss,Sd) -> T = "yes" ; T = "no"),
    writeln(T).
main.
*/