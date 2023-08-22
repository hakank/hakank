% https://open.kattis.com/problems/apaxiaaans
% Time limit: 1s
% Diff 1.4 Easy

% TODO: This is accepted but way too complex...
% But it's the shortest of the three Prolog entries:
% 538 chars. The other two are 1363 chars and 1700 chars.
% In apaxiaaans2.pl clumped/2 is used instead: 128 chars.

main :-
    read_line_to_codes(user_input,S),
    remove_duplicates(S,[],Res),
    format('~s~n',[Res]).
main.

remove_duplicates([],S,S).
remove_duplicates([C],S,[C|S]).
remove_duplicates([C,C|Cs],S0,[C|S]) :-
    remove_all(C,Cs,[],Cs2),
    remove_duplicates(Cs2,S0,S).
remove_duplicates([C,D|Cs],S0,[C|S]) :-
    C \= D,
    remove_duplicates([D|Cs],S0,S).

remove_all(_,[],S,S).
remove_all(C,[C],S,S).
remove_all(_,[C],S,[C|S]).
remove_all(C,[C|Cs],S0,S) :-
    remove_all(C,Cs,S0,S).
remove_all(C,[D|Cs],_S0,[D|Cs]) :-
    C \= D.
    