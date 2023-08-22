% https://open.kattis.com/problems/missingnumbers
% 1s
% 1.6 Easy

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    last(Ns,Last),
    findall(I,(between(1,Last,I),\+memberchk(I,Ns)),Is),
    (Is == [] -> writeln("good job");maplist(writeln,Is)).
main.
