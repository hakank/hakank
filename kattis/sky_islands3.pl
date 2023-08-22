% https://open.kattis.com/problems/skyislands
% 6s (!)
% 2.0 Easy

% Using readln/2 instead: 248 chars.
% NOPE!
% Darn, it's slower than sky_islands2.pl: Time Limit Exceeded on 6/7!

:- use_module(library(ugraphs)).
main:-readln([N,_|Ns],end_of_file),(N=<1->T="YES";s(Ns,P),vertices_edges_to_ugraph([],P, G),reachable(1,G,R), length(R,Len),(Len>=N-1 -> T="YES";T="NO")),writeln(T).
s([],[]). s([N1,N2|Ns],[N1-N2,N2-N1|P]):-s(Ns,P).

/*
% Uncompressed: 386 chars
:- use_module(library(ugraphs)).
main :- 
    readln([N,_|Ns],end_of_file),
    (N =< 1 -> 
        T="YES"
    ;
        s(Ns,P),
        vertices_edges_to_ugraph([],P, G),
        reachable(1,G,R),
        length(R,Len),
        (Len >= N-1 ->
            T = "YES"
        ;
            T="NO"
        )
    ),
    writeln(T).
s([],[]).
s([N1,N2|Ns],[N1-N2,N2-N1|P]) :-
    s(Ns,P).
*/
