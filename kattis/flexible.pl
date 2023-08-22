% https://open.kattis.com/problems/flexible
% 1s
% 1.8 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[W,_|Ns0],Ss),
    append(Ns0,[0,W],Ns),
    findall(D,(member(N1,Ns),member(N2,Ns),D is abs(N1-N2)),Ds),
    sort(Ds,[0|Sol]),
    maplist(format('~d '),Sol),
    nl.

    