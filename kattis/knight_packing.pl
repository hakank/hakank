% https://open.kattis.com/problems/knightpacking
% 1s
% 1.3 Easy

% See knight_packing.pi for some thinking on this

main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    (ceiling(N*N/2) mod 2 =:= 1 ->
        T="first"
    ;
        T="second"
    ),
    writeln(T).
