% https://open.kattis.com/problems/loorolls
% 1s
% 1.7 Easy

% TODO!

main :-
    read_string(user_input,100000,S),
    writeln(s=S),
    split_string(S," ","\n",Ss),
    writeln(ss=Ss),
    maplist(number_string,[L,N],Ss),
    writeln([l=L,n=N]),
    Div is L div N,
    writeln(div=Div),
    Mod is L mod N,
    writeln(mod=Mod),
    
    nl.
main.
