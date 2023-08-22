% https://open.kattis.com/problems/laptopsticker
% 1s 2048Mb
% 1.5 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[WC,HC,WS,HS],Ss),
    ((2+WS =< WC, 2+HS =< HC) -> T = 1 ; T = 0),
    writeln(T).

