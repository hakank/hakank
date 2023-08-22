% Problem https://open.kattis.com/problems/different
% Solution from https://open.kattis.com/help/prolog
% CPU time limit: 1s
% Difficulty: 2.6 Easy

% w/o kattio.pl

main :-
    repeat,
    read_string(S),
    (S == end_of_file ;
        split_string(S," ", "", Ss),
        maplist(number_string,[X,Y],Ss),
        Z is abs(X-Y),
        writeln(Z),
        fail
    ).

read_string(S) :-
    read_line_to_string(user_input,S).
