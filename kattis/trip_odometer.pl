% https://open.kattis.com/problems/tripodometer
% 1s
% 2.0 Easy

main :-
    readln([_|Ns],end_of_file),
    writeln(ns=Ns),
    sum_list(Ns,Sum),
    s(Ns,Sum,[],L),
    sort(L,Ls),
    writeln(ls=Ls),
    length(Ls,Len),
    writeln(Len),
    maplist(format("~d "),Ls),
    nl.
s([],_,L,L).
s([N|Ns],Sum,L0,[V|L]) :-
    V is Sum-N,
    s(Ns,Sum,L0,L).

/*
% Compressed: 198 chars
main:-readln([_|Ns],end_of_file),sum_list(Ns,Sum),s(Ns,Sum,[],L),sort(L,Ls),length(Ls,Len),
writeln(Len),maplist(format("~d "),Ls),nl.
s([],_,L,L). s([N|Ns],Sum,L0,[V|L]):-V is Sum-N,s(Ns,Sum,L0,L).
*/


/*
main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,Sum),
    s(Ns,Sum,[],L),
    sort(L,Ls),
    length(Ls,Len),
    writeln(Len),
    maplist(format("~d "),Ls),
    nl.
s([],_,L,L).
s([N|Ns],Sum,L0,[V|L]) :-
    V is Sum-N,
    s(Ns,Sum,L0,L).
*/

/*
% This is too slow.
mainx :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    findall(V,(select(_,Ns,Ns2), sum_list(Ns2,V)),Vs),
    sort(Vs,SS),
    length(SS,Len),
    writeln(Len),
    maplist(format("~d "),SS),
    nl.
*/