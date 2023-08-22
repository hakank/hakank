% https://open.kattis.com/problems/oddities
% Time limit: 1s
% Diff 1.3 Easy


main :-
    read_string(_),
    repeat,
    read_string(S),
    (S == end_of_file ;
        number_string(N,S),
        (N mod 2 =:= 0 -> 
            format('~d is even~n',[N])
        ;
            format('~d is odd~n',[N])
        ),
        fail
    ).
main.

read_string(S) :-
    read_line_to_string(user_input,S).
