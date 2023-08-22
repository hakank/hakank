% https://open.kattis.com/problems/nothanks
% 1s
% 2.0 Easy

% See no_thanks.py for a shorter variant.

% Another approach: Accepted (0.44s)
% Shorter using two readln/1 instead of one readln/2
main :-
    readln(_),    
    readln(T),
    sort(T,S),
    [F|L] = S,    
    s(L,F,F,R),
    writeln(R).

s([],_,S,S).
s([H|T],F,S0,S) :-
    (H > F+1 ->
        S1 is S0+H
    ;
        S1 is S0
    ),
    s(T,H,S1,S).

/* 
% Compressed: 144 chars (not short enough for Top 10 (66..141)
main:-readln([_|T],end_of_file),sort(T,S),[F|L]=S,s(L,F,F,R),writeln(R).
s([],_,S,S). s([H|T],F,S0,S):-(H>F+1->S1 is S0+H;S1 is S0),s(T,H,S1,S).

% Shorter: 138 chars. Was in Top 10 place 10 until I go my Python3 solution (90 chars)
% in on place 4...
main:-readln(_),readln(T),sort(T,S),[F|L]=S,s(L,F,F,R),writeln(R).
s([],_,S,S). s([H|T],F,S0,S):-(H>F+1->S1 is S0+H;S1 is S0),s(T,H,S1,S).

*/


/*
% Time Limit Exceeded on 6/10
main :-
    readln([_|T],end_of_file),
    sort(T,S),
    [F|L] = S,
    r(L,F,[F],[],R),
    maplist(min_list,R,M),
    sum_list(M,Sum),
    writeln(Sum).
r([],_,R,L,[R|L]).
r([A|T],E0,T0,L0,L) :-
    A is E0+1,
    append(T0,[A],T1),
    r(T,A,T1,L0,L).
r([A|T],E0,T0,L0,[T0|L]) :-
    A > E0+1,
    r(T,A,[A],L0,L).
*/