% https://open.kattis.com/problems/baconeggsandspam
% 1s
% 1.8 Easy

% This is messy.
% 688 chars.
% Cf with the Picat version (using maps): 367 chars

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).

s([]).
s(["0"]).
s([S|Ss]) :-
    number_string(N,S),
    length(L,N),
    append(L,Ss2,Ss),
    maplist(sp,L,Ls),
    findall(F,member([_,F],Ls),Fs),
    flatten(Fs,Fs2),
    sort(Fs2,FSorted),
    findall(P,member([P,_],Ls),Ps),
    sort(Ps,PSorted),
    findall(F-PPs,(member(F,FSorted),
                   findall(P,(member(P,PSorted),member([P,Food],Ls),member(F,Food)),PPs)
                  ),
        FPs),
    maplist(p,FPs),
    nl,
    s(Ss2).

p(Food-Who) :-
    write(Food),write(" "),
    maplist(format("~s "),Who),
    nl.

sp(S,[Name,Ls]) :-
    split_string(S," ","",[Name|Ls]).