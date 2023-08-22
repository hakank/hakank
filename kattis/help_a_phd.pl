% https://open.kattis.com/problems/helpaphd
% 1s
% 1.7 Easy

% Testing read_from_chars/2 that I just found (see README.hkj).

main:-read_string(user_input,10000,S),split_string(S,"\n","\n",[_|Ss]),s(Ss).
s([]). s(["P=NP"|Ss]):-w(skipped),s(Ss). s([S|Ss]):-read_from_chars(S,T),X is T,w(X),s(Ss).
w(S):-writeln(S).
