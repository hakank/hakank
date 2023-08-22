% https://open.kattis.com/problems/grading
% 1s
% 1.6 Easy

% Shorter version (but not compressed).
% Note that split string can split both "\n" and " " at the same time!

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n ","\n ",X),
    maplist(number_string,Xs,X),
    append(Ns0,[V],Xs),
    append(Ns0,[0],Ns),
    g(["A","B","C","D","E","F"],Ns,V).
main.
g([],_,_).
g([G|Gs],[N|Ns],V) :-
    (V >= N ->
        writeln(G)
    ;
        g(Gs,Ns,V)
    ).
