% https://open.kattis.com/problems/server
% 1s
% 1.7 Easy

main:-read_string(user_input,100000,S),split_string(S,"\n ","\n ", Ss),
maplist(number_string,[_,T|Ns],Ss),s(Ns,T,0,0,C),writeln(C).
s([],_,_,C,C).
s([N|Ns],T,S,C0,C):-S1 is S + N,(S1=<T->C1 is C0 + 1;C1 is C0),s(Ns,T,S1,C1,C).