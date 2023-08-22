% https://open.kattis.com/problems/ofugsnuid
% CPU Time limit: 1s
% Difficulty 1.3-1.5 Easy
%
:- [kattio].

main :-
    read_int(NumLines),
    collect_lines_int(Lines),
    length(Lines,NumLines),
    reverse(Lines,Reversed),
    print_lines(Reversed).

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
