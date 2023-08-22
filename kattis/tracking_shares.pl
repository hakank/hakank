% https://open.kattis.com/problems/trackingshares
% 1s
% 2.1 Easy

% This was not very fun ... well, ok a little fun.
% It uses a plain list [Company1-Shares1,Company2-Shares2,...]
% for representing the shares which is then updated for each day.
% But before that a lot of preprocessing has to be done (and that's
% the unfun part).
% 832 chars
% I'm not sure if using dicts would have made this much smaller...

% (The Picat version - using map and before tweaking too much -
% is shorter: 486 chars)

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[NC0|Ss]),
    number_string(NC,NC0),
    s(1,Ss,T),
    flatten(T,F),
    msort(F,Fs),
    findall(D,member(D-_,Fs),Ds),
    sort(Ds,DsS),
    findall(C-0,between(1,NC,C),Cs),
    f(DsS,Fs,Cs),
    nl.

f([],_,_).
f([D|Ds],Fs,Cs0) :-
    findall(C-N,member(D-[C,N],Fs),Ts),
    c(Ts,Cs0,Cs2),   
    findall(N,member(_-N,Cs2),Ns),
    sum_list(Ns,Sum),
    format("~d ",[Sum]),
    f(Ds,Fs,Cs2).

c([],Cs,Cs).
c([C-N|CNs],Cs0,Cs) :-
    select(C-_,Cs0,Cs1),
    append([C-N],Cs1,Cs2),
    c(CNs,Cs2,Cs).   

s(_,[],[]).
s(C,[S|Ss],[AsS|L]) :-
    number_string(N,S),
    length(A,N),
    append(A,Rest,Ss),
    maplist(p(C),A,As),
    msort(As,AsS),
    C1 is C+1,
    s(C1,Rest,L).

p(C,L,D-[C,N]) :- split_string(L," ","",ND),maplist(number_string,[N,D],ND).
