% https://open.kattis.com/problems/secretmessage
% 1s
% 1.7 Easy

%% ilov
%% eyou
%% Jack
%% ****

%% *Jei
%% *ayl
%% *coo
%% *kuv

% --> Jeiaylcookuv

:- use_module(library(clpfd)).
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_codes,Ss,Cs),
    s(Cs).

s([]).
s([S|Ss]) :-
    length(S,Len),
    N is ceil(sqrt(Len)),
    (Len < N*N  ->
        Diff is N*N - Len,
        findall(0'*,between(1,Diff,_),D),
        append(S,D,S2)
    ;
        S2 = S
    ),
    part(S2,N,Cs),
    rotate(Cs,CsT),
    maplist(f,CsT),
    nl,
    s(Ss).

f(L) :-
    delete(L,0'*,L2),    
    string_codes(S,L2),
    format('~s',S).

rotate(Xss, Zss) :-
   transpose(Xss, Yss),
   maplist(reverse, Yss, Zss).

part([], _, []).
part(L, N, [DL|DLTail]) :-
   length(DL, N),
   append(DL, LTail, L),
   part(LTail, N, DLTail).
