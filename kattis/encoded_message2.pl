% https://open.kattis.com/problems/encodedmessage
% Time limit: 1s
% Diff: 1.5 Easy

:- use_module(library(clpfd)).
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_codes,Ss,Cs),
    maplist(decode,Cs).
decode(S) :-
    string_length(S,Len),
    N is ceiling(sqrt(Len)),
    part(S,N,Chunks),
    transpose(Chunks,T),
    reverse(T,TR),
    append(TR,SS),
    format('~s~n',[SS]).
part([], _, []).
part(L, N, [DL|DLTail]) :-
   length(DL, N),
   append(DL, LTail, L),
   part(LTail, N, DLTail).
