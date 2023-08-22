% https://open.kattis.com/problems/apaxianparent
% 1s
% 1.6 Easy

% Run time error. Probably regex that's not supported!?
% :- use_module(library(pcre)).
% Ah, regexes are not supported in 8.4.2.
% https://www.swi-prolog.org/download/stable/doc/SWI-Prolog-8.4.2.pdf

main :-
    % read_string(user_input,100,S),
    read_line_to_codes(user_input,S),
    % re_matchsub("^([^ ]+?)(.)(.) (.+)$",S,Sub),
    % s(Sub.1,Sub.2,Sub.3,Sub.4,R),
    s(S,R),
    writeln(r=R),
    atomics_to_string(R,Res),
    writeln(Res).
main.

s([A,0'e,0'x,B],[A,"ex",B]):-!.
s([A,C1,C2,B],[A,C1,"ex",B]) :- memberchk(C2,[0'a,0'i,0'o,0'u]),!.
s([A,C1,0'e,B],[A,C1,"ex",B]):-!.
s([A,C1,C2,B],[A,C1,C2,"ex",B]).
