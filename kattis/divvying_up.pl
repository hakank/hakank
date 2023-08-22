% https://open.kattis.com/problems/divvyingup
% 1s
% 1.6 Easy

main :-
    rs(N0),rs(S),
    number_string(N,N0),
    split_string(S," ","", Ss),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    (0 is Sum mod 3 -> T = yes ; T = no),
    writeln(T).
main.
rs(S) :- read_line_to_string(user_input,S).
    