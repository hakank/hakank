% https://open.kattis.com/problems/tajna
% 1s
% 2.1 Easy

:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,S),
    string_chars(S,Cs),
    length(Cs,Len),
    findall(R-C,(between(1,Len,C),between(1,C,R),R*C=:=Len),Lens),
    sort(1,@>=,Lens,[Rows-_|_]),
    (Rows =:= 1 ->
        RC=Cs
    ;
        part(Cs,Rows,M),
        transpose(M,T),
        flatten(T,RC)
    ),
    string_chars(R,RC),
    writeln(R).

part([], _, []).
part(L, N, [DL|DLTail]) :-
    length(DL, N),
    append(DL, LTail, L),
    part(LTail, N, DLTail).
