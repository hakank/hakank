% https://open.kattis.com/problems/thisaintyourgrandpascheckerboard
% 1s
% 1.7 Easy

% clpfd is only used for transpose/2,
% The compressed version (317 chars) actually came in as #9 on the
% Top 10 shortest programs for all languages!
%
% Changing read_string/3 to readln/2: even shorter: 265 chars -> #4 on Top 10.

:- use_module(library(clpfd)).
main:-readln([_|S],end_of_file),maplist(string_codes,S,C),transpose(C,CT),((s(C),s(CT))->T=1;T = 0),writeln(T).
s([]). s([H|T]):-findall(1,member(0'B,H),Bs),findall(1,member(0'W,H),Ws),same_length(Bs,Ws),\+ append(_,[X,X,X|_],H),s(T).

/*
% Uncompressed: 341 chars
:- use_module(library(clpfd)).
main :-
    readln([_|S],end_of_file),
    maplist(string_codes,S,C),
    transpose(C,CT),
    ((s(C),s(CT)) -> 
         T=1
    ;
         T = 0),
    writeln(T).
s([]). 
s([H|T]) :- 
    findall(1,member(0'B,H),Bs),findall(1,member(0'W,H),Ws),
    same_length(Bs,Ws),
    \+ append(_,[X,X,X|_],H),
    s(T).
*/