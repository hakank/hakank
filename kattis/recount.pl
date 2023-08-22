% https://open.kattis.com/problems/recount
% 2s
% 2.1 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",T),
    append(X,["***"],T),
    msort(X,Y),
    clumped(Y,Cl),
    sort(2,@>=,Cl,[A-AN,_-BN|_]),
    (AN>BN -> writeln(A) ; writeln("Runoff!")).

/*
% Compressed: 170 chars (place 6 at Top 10!)
main:-read_string(user_input,_,S),split_string(S,"\n","\n",T),append(X,["***"],T),msort(X,Y),
clumped(Y,C),sort(2,@>=,C,[A-P,_-Q|_]),(P>Q->writeln(A);writeln("Runoff!")).
*/