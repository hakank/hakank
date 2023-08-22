% https://open.kattis.com/problems/finalexam2
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    findall(A,nextto(A,A,Ss),L),
    length(L,Len),
    writeln(Len).
