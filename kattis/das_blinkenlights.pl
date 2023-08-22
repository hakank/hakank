% https://open.kattis.com/problems/dasblinkenlights
% 1s
% 1.7 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[P,Q,Max],Ss),
    (lcm(P,Q) =< Max -> T=yes ; T = no),   
    writeln(T).
