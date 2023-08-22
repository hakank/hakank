% https://open.kattis.com/problems/bela
% Time: 1s
% Difficulty: 1.3 Easy

% Shorter variant w/o kattio.pl
% :- [kattio].

main :-
    read_line_to_string(user_input,FirstLine),
    split_string(FirstLine," ","", [_,TrumphS]),
    atom_string(Trumph,TrumphS),
    check_cards(Trumph,Sum),
    writeln(Sum).
main.

check_cards(Trumph,Counts) :-
    read_atom(Card),
    check_card(Trumph,Card,0,Counts).

check_card(_Trumph,end_of_file,Counts,Counts).
check_card(Trumph,Card,Counts0,Counts) :-
    atom_chars(Card,[Value,Suite]),
    value(Value,Dom,NonDom),
    (Trumph == Suite ->
        Count = Dom
    ;
        Count = NonDom
    ),
    read_atom(Card2),
    Counts1 is Counts0 + Count,
    check_card(Trumph,Card2,Counts1,Counts).


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

read_atom(Atom) :-
    read_line_to_string(user_input,In),
    atom_string(Atom,In).
        