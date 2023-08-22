% https://open.kattis.com/problems/sibice
% CPU Time limit: 1s
% Difficulty: 1.4 Easy

% Using read_string/3

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ", "\n ", Ss),
    maplist(number_string,[_,W,H|Ns],Ss),
    s(Ns,W,H).
s([],_,_).
s([S|Ss],W,H):-(S=<sqrt(W^2+H^2)->T="DA";T="NE"), writeln(T),s(Ss,W,H).
