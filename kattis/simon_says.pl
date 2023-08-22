% https://open.kattis.com/problems/simonsays
% 1s
% 1.6 Easy

main:-rs(_),ra(L),s(L). main.
s([]). s([S|T]):-(atom_concat("Simon says",X,S)->writeln(X);true),s(T).
ra(S):-rs(In),ra(In,[],S).
ra(end_of_file,S,S).
ra(In,S0,[In|S]):-rs(S2),ra(S2,S0,S).
rs(S):-read_line_to_string(user_input,S).
    
    