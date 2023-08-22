% https://open.kattis.com/problems/deathknight
% 1s
% 1.6 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_chars,Ss,Cs),
    findall(C,(member(C,Cs),\+c(C)),Res),
    length(Res,Len),
    writeln(Len).
main.
c(C) :- append(_,['C','D'|_],C).