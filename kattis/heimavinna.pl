% https://open.kattis.com/problems/heimavinna
% 1s
% 1.1-1.6 Easy


main :-
    read_line_to_string(user_input,S),
    split_string(S,";","",L),
    c(L,0,C),
    writeln(C).
main.

c([],C,C).
c([H|T],C0,C) :-
    (split_string(H,"-","",[From0,To0]) ->
        maplist(number_string,[From,To],[From0,To0]),
        V is To - From + 1
    ;
        V = 1
    ),
    C1 is C0 + V,
    c(T,C1,C).
