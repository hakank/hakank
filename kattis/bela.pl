% https://open.kattis.com/problems/bela
% Time: 1s
% Difficulty: 1.3 Easy

:- [kattio].

main :-
    read_int(_NumHands), read_atom(Trumph),
    collect_lines_atom(Cards),
    maplist(check_card(Trumph),Cards,Counts),
    sum_list(Counts,Sum),
    writeln(Sum).
main.

check_card(Trumph,Card,Count) :-
    atom_chars(Card,[Value,Suite]),
    value(Value,Dom,NonDom),
    (Trumph == Suite ->
        Count = Dom
    ;
        Count = NonDom
    ).


%  Number Value
%         Dominant Not dominant
value('A',11,11).
value('K',4,4).
value('Q',3,3).
value('J',20,2).
value('T',10,10).
value('9',14,0).
value('8',0,0).
value('7',0,0).


collect_lines_string(Lines) :-
    read_string(X),
    collect_lines_string(X,[],Lines).

collect_lines_string(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_string(X,Lines0,[X|Lines]) :-
    read_string(X2),
    collect_lines_string(X2,Lines0,Lines).


% For atoms
collect_lines_atom(Lines) :-
    read_atom(X),
    collect_lines_atom(X,[],Lines).

collect_lines_atom(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_atom(X,Lines0,[X|Lines]) :-
    read_atom(X2),
    collect_lines_atom(X2,Lines0,Lines).

