% https://open.kattis.com/problems/titlecost
% 1s
% 1.9 Easy


main :-
    readln([T,C]),
    string_length(T,Len),
    format("~8f~n",[min(C,Len)]).

/*
% Compressed: 70 chars
main:-readln([T,C]),string_length(T,Len),format("~8f~n",[min(C,Len)]).

*/

/*
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",[T,C0]),
    number_string(C,C0),
    string_length(T,Len),
    V is min(C,Len),
    format("~w~n",[V]).
*/