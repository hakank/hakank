% https://open.kattis.com/problems/batterup
% 1s
% 1.3 Easy

main :-
    rs(_),
    rs(S),
    split_string(S," ","",Ss),
    maplist(number_string,Ns,Ss),
    findall(N,(member(N,Ns),N>=0),N2),
    sum_list(N2,Sum),
    length(N2,Len),
    A is Sum/Len,
    writeln(A).
main.

rs(S) :- read_line_to_string(user_input,S).