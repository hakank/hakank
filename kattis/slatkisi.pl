% https://open.kattis.com/problems/slatkisi
% 1s
% 2.0 Easy

% 155 chars
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[C,K],Ss),
    V is round(C/10^K)*10^K,writeln(V).

% 167 chars
main0 :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[C,K],Ss),
    T is 10^K,D is round(C/T),V is D*T,writeln(V).
