% https://open.kattis.com/problems/grading
% 1s
% 1.6 Easy

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n","\n",[Ls0,V0]),
    split_string(Ls0," ","",Ls1),
    maplist(number_string,Ns0,Ls1),
    append(Ns0,[0],Ns),
    number_string(V,V0),
    g(["A","B","C","D","E","F"],Ns,V).
main.

g([],_,_).
g([G|Gs],[N|Ns],V) :-
    (V >= N ->
        writeln(G)
    ;
        g(Gs,Ns,V)
    ).
