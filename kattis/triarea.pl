% https://open.kattis.com/problems/triarea
% 1s
% 1.5 Easy

main :-
    read_string(S),
    split_string(S," ","",Ss),
    maplist(number_string,[A,B],Ss),
    T is A*B/2,
    format('~10f~n',[T]).
main.

read_string(S) :- read_line_to_string(user_input,S).