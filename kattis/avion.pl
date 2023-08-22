% https://open.kattis.com/problems/avion
% 1s
% 1.4 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(ls(Lines),user_input),
    length(Lines,Len),
    findall(I,(between(1,Len,I),nth1(I,Lines,Line),append(_,[0'F,0'B,0'I|_],Line),write(I),write(" ")),Is),
    (Is == [] -> writeln("HE GOT AWAY!") ; nl).

main.

ls([L|Ls]) --> string_without("\n",L),{L\=[]},eol,ls(Ls).
ls([]) --> [].
