% https://open.kattis.com/problems/averagecharacter
% 1s 2048MB
% 1.9 Easy

main :-
    read_line_to_codes(user_input,S),
    sum_list(S,Sum), length(S,Len),A is floor(Sum/Len),
    format("~c~n",[A]).

/*
% Compressed: 108 chars
main:-read_line_to_codes(user_input,S),sum_list(S,Sum),length(S,Len),A is floor(Sum/Len),format("~c~n",[A]).
.
*/