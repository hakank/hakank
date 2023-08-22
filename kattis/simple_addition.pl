% https://open.kattis.com/problems/simpleaddition
% 1s
% 2.2 Easy

% The high difficulty is probably due to arbitrary precision

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[A,B]),
    string_concat(A,"+",A1),
    string_concat(A1,B,SS),
    read_from_chars(SS,T),
    C is T.
    format('~w~n',[C]).



main_old :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(number_string,[A,B],Ss),
    C is A+B,
    format('~w~n',[C]).

    