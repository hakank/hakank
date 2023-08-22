% https://open.kattis.com/problems/bookingaroom
% 1s
% 1.8 Easy
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[R,_],Ss),
    once(read_all(Rooms)),
    findall(F, (between(1,R,F),
                   not(memberchk(F,Rooms))
                   ),Free),
    length(Free,Len),    
    (Len > 0 ->
        [H|_] = Free,
        writeln(H)
    ;
        writeln("too late")
    ).
main.

read_all(S) :-
    read_string(In),
    read_all(In,[],S).
read_all(end_of_file,S,S).
read_all(In,S0,[N|S]) :-
    number_string(N,In),
    read_string(S2),
    read_all(S2,S0,S).

read_string(S) :-
    read_line_to_string(user_input,S).
