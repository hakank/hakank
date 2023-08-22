% https://open.kattis.com/problems/railroad2
% 1s
% 1.5 Easy

% TODO

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",[_,YS]),
    number_string(Y,YS),
    writeln(y=Y),
    M is 3-Y mod 2,
    writeln(m=M),
    (3-Y mod 2 =:= 0 -> T="possible" ;T="impossibe"),
    writeln(T).

