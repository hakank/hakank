% https://open.kattis.com/problems/cutinline
% 1s
% 1.7s

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[N0|Ss]),
    number_string(N,N0),
    length(People,N),
    append(People,[_|Instructions],Ss),
    s(Instructions,People,Res),
    maplist(writeln,Res).
    
s([],People,People).
s([I|Is],People0,People) :-
    split_string(I," ","",[Ins|Rest]),
    (Ins == "leave" ->
        [A] = Rest,
        leave(A,People0,People1)
    ;
        Rest = [A,B],
        cut(A,B,People0,People1)
    ),
    s(Is,People1,People).
    
cut(A,B,PeopleIn,PeopleOut) :-
    append(Pre,[B|Rest],PeopleIn),
    append(Pre,[A,B|Rest],PeopleOut).
leave(A,PeopleIn,PeopleOut) :-
    delete(PeopleIn,A,PeopleOut).


            