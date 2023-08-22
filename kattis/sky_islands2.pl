% https://open.kattis.com/problems/skyislands
% 6s (!)
% 2.0 Easy

% Using ugraphs: https://www.swi-prolog.org/man/ugraphs.html

% This works. One issue was that a single island
% should be considered to be connected.

% 326 chars. Place 4 in Top 10.
:- use_module(library(ugraphs)).
main :- read_string(user_input,_,S), split_string(S,"\n ","\n ",Ss),
maplist(number_string,[N,_|Ns],Ss),
(N=<1 -> T="YES"; s(Ns,P),vertices_edges_to_ugraph([],P, G),reachable(1,G,R), length(R,Len),(Len >= N-1 -> T="YES"; T="NO")),writeln(T).
s([],[]). s([N1,N2|Ns],[N1-N2,N2-N1|P]) :- s(Ns,P).

/*
% Uncompressed: 428 chars
:- use_module(library(ugraphs)).
main :- 
    read_string(user_input,_,S), 
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,_|Ns],Ss),
    (N=<1 -> T="YES"
    ;
        s(Ns,P),
        vertices_edges_to_ugraph([],P, G),
        reachable(1,G,R),
        length(R,Len),
        (Len >= N-1 -> T="YES"
        ;
            T="NO"
        )
    ),writeln(T).
s([],[]).
s([N1,N2|Ns],[N1-N2,N2-N1|P]) :- s(Ns,P).
*/
