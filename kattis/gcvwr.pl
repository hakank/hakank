% https://open.kattis.com/problems/gcvwr
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S1),
    split_string(S1," ","",Ss1),
    maplist(number_string,[G,T,_N],Ss1),
    read_line_to_string(user_input,S2),
    split_string(S2," ","",Ss2),
    maplist(number_string,L,Ss2),    
    GT is G-T,
    GT2 is 0.9*GT,
    sum_list(L,Sum),
    X is round(GT2 - Sum),
    writeln(X).
main.
