% https://open.kattis.com/problems/ofugsnuid

% Trying to make a shorter program.

:- [kattio].

main :-
    read_int(NumLines),
    repeat,
    read_int(X),
    L = [],
    (X == end_of_file ;
        writeln(x=X),
        append(X,L,L),
        fail
    ),
    writeln(l=L).

c([],[]).
c([H|T],[H|L]) :-
    c(T,L).

print_lines([]).
print_lines([Line|Lines]) :-
    writeln(Line),
    print_lines(Lines).


% For ints
collect_lines_int(Lines) :-
    read_int(X),
    collect_lines_int(X,[],Lines).

collect_lines_int(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_int(X,Lines0,[X|Lines]) :-
    read_int(X2),
    collect_lines_int(X2,Lines0,Lines).
