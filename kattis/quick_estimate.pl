% https://open.kattis.com/problems/quickestimate
% 1s
% 1.6 Easy

main:-rs(_),ra(L),q(L).
main.
q([]).
q([I|Is]):-number_string(N,I),
    (N > 0 -> 
        T is 1+floor(log(N)/log(10))
    ;
        T is 1
    ),
    writeln(T),
    q(Is).

ra(S):-rs(In),ra(In,[],S).
ra(end_of_file,S,S).
ra(In,S0,[In|S]):-rs(S2),ra(S2,S0,S).
rs(S):-read_line_to_string(user_input,S).

    