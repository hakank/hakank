% https://open.kattis.com/problems/compoundwords
% 1s
% 1.8 Easy

main :-
    read_string(user_input, 1000000000,S),
    split_string(S,"\n ","\n ", Ss),
    maplist(string_chars,Ss,Cs),
    findall(W,(member(C1,Cs),member(C2,Cs),C1\=C2,append(C1,C2,W)),Ws),
    sort(Ws,Sol1),
    maplist(string_chars,Sol,Sol1),
    maplist(writeln,Sol).
