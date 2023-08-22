% https://open.kattis.com/problems/bookingaroom
% 1s
% 1.8 Easy
main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[R,_|Rooms],Ss),
    findall(F,(between(1,R,F),not(memberchk(F,Rooms))),Free),
    length(Free,Len),    
    (Len > 0 -> [H|_] = Free,T=H; T="too late"),
    writeln(T).
