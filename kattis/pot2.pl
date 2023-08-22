% https://open.kattis.com/problems/pot
% 1s
% 1.4 Easy

% Shorter (and silly) version
main:-ri(_),ra(R),format('~d~n',[R]). main.
ra(A):-rs(I),ra(I,0,A).
ra(end_of_file,S,S).
ra(In,S0,S):-ns(N,In),S1 is S0+(N div 10)^(N mod 10),rs(S2),ra(S2,S1,S).
rs(S):-read_line_to_string(user_input,S).
ri(N):-rs(I),ns(N,I).
ns(N,I):-number_string(N,I).