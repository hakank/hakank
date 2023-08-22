% https://open.kattis.com/problems/unlockpattern
% 1s
% 1.7 Easy

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,Ns,Ss),
    findall(P,(between(1,9,I),nth1(P,Ns,I)),Ps),
    findall(D,(nextto(A,B,Ps),d(A,B,D)),Ds),
    sum_list(Ds,Sum),
    writeln(Sum).
d(A,B,D) :-
    A1 is A-1,B1 is B-1,
    AI is A1 div 3, AJ is A1 mod 3,
    BI is B1 div 3, BJ is B1 mod 3,
    D is sqrt((AI-BI)^2+(AJ-BJ)^2).    
