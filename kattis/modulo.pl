% https://open.kattis.com/problems/modulo
% 1s
% 1.6 Easy

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n"," ",Ss),
    append(T,[""],Ss),
    maplist(number_string,Ns,T),
    findall(M,(member(I,Ns),M is I mod 42),Ms),
    sort(Ms,Sorted),
    length(Sorted,Len),
    writeln(Len).
main.
