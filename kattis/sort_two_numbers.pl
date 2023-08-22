% https://open.kattis.com/problems/sorttwonumbers
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[A,B],Ss),
    (A =< B -> 
        format('~d ~d~n',[A,B])
    ;
        format('~d ~d~n',[B,A])
    ).
main.
