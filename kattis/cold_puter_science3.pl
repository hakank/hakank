% https://open.kattis.com/problems/cold
% CPU Time Limit: 1s
% Difficulty: 1.3 Easy

% Making it a little shorter: 198 chars

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n ","\n",[_|Ss]),    
    maplist(number_string,Ns,Ss),
    findall(V,(member(V,Ns),V<0),Vs),
    length(Vs,Len),
    writeln(Len).
