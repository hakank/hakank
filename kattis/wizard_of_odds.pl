% https://open.kattis.com/problems/wizardofodds
% 1s
% 2.5 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[N,K],Ss),
    L is log10(N)/log10(2),
    (K >= L -> T="Your wish is granted!";T="You will become a flying monkey!"),
    writeln(T).
