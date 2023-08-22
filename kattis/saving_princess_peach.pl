% https://open.kattis.com/problems/princesspeach
% 1s
% 2.1 Easy

main :-
    readln([N,_|Ns0],end_of_file),
    sort(Ns0,Ns),
    N1 is N-1,
    findall(I,(between(0,N1,I),not(memberchk(I,Ns))),Is),
    maplist(writeln,Is),
    length(Is,Len),
    V is N-Len,
    format("Mario got ~d of the dangerous obstacles.~n",[V]).

/*
% Compressed: 210 chars (not enough for Top 10: 92..187 chars)
main:-readln([N,_|Ns0],end_of_file),sort(Ns0,Ns),N1 is N-1,
findall(I,(between(0,N1,I),not(memberchk(I,Ns))),Is),maplist(writeln,Is),length(Is,Len),
format("Mario got ~d of the dangerous obstacles.~n",[N-Len]).
*/