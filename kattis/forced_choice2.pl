% https://open.kattis.com/problems/forcedchoice
% 1s
% 1.4 Easy

% Much better: avg time 0.03s

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[F|Ss]),
    split_string(F," ","",[_,PS,_]),
    s(Ss,PS).

s([],_).
s([S|Ss],P) :-
    split_string(S," ","",[_|R]),
    (memberchk(P,R) -> V = "KEEP" ; V = "REMOVE"),
    writeln(V),
    s(Ss,P).
