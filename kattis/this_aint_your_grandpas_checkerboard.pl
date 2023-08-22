% https://open.kattis.com/problems/thisaintyourgrandpascheckerboard
% 1s
% 1.7 Easy

% clpfd is only used for transpose/2,
% The compressed version (317 chars) actually came in as #9 on the
% Top 10 shortest programs for all languages!

:- use_module(library(clpfd)).
main:-read_string(user_input,10000,S),split_string(S,"\n","\n",[_|Ss]),maplist(atom_codes,Ss,Cs),(s(Cs)->transpose(Cs,CsT),(s(CsT)->T=1;T=0);T=0),writeln(T). main.
s([]). s([H|T]):-findall(1,member(0'B,H),Bs),findall(1,member(0'W,H),Ws),same_length(Bs,Ws),\+ append(_,[X,X,X|_],H),s(T).


% Here's the program uncompressed:
/*
:- use_module(library(clpfd)).
main :- 
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(atom_codes,Ss,Cs),
    (s(Cs)->
        transpose(Cs,CsT),
        ( s(CsT) ->
            T = 1
        ;
            T = 0
        )
    ;
        T = 0),
    writeln(T).
main.
s([]).
s([H|T]) :-
    findall(1,member(0'B,H),Bs),
    findall(1,member(0'W,H),Ws),
    same_length(Bs,Ws),
    \+ append(_,[X,X,X|_],H),
    s(T).
*/