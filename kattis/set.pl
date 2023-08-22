% https://open.kattis.com/problems/set
% 1s
% 2.1 Easy

% Indices:
%  1  2  3
%  4  5  6
%  7  8  9
% 10 11 12
%

% 650 chars

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(string_chars,Ss,T),
    length(T,Len),
    findall([A,B,C],(
        between(1,Len,A),A1 is A+1,
        between(A1,Len,B),B1 is B+1,
        between(B1,Len,C),
        s(T,A,B,C)
    ),Sets),
    (Sets == [] ->
        writeln("no sets")
    ;
        sort(Sets,R),
        maplist(w,R)
    ).
w([A,B,C]) :- format("~w ~w ~w~n",[A,B,C]).
s(T,A,B,C) :-
    nth1(A,T,SA),nth1(B,T,SB),nth1(C,T,SC),
    once(t(SA,SB,SC)).
t([],[],[]).
t([A|As],[B|Bs],[C|Cs]) :-
    (a([A,B,C]) ; d([A,B,C])),
    t(As,Bs,Cs).
a([A,A,A]).
d([A,B,C]) :- dif(A,B),dif(A,C),dif(B,C).
