% https://open.kattis.com/problems/haypoints
% 1s
% 1.9 Easy

% TODO: Just parsing this will take a lot of time...
% The Picat version is much neater: haypoints.pi

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[MN|Ss]),
    split_string(MN," ","",[M0,N0]),
    maplist(number_string,[M,N],[M0,N0]),
    writeln([m=M,n=N]),    
    length(Ws0,M),
    append(Ws0,Js0,Ss),    
    writeln(ws0=Ws0),
    ws(Ws0,Ws),
    writeln(ws=Ws),
    nl,
    % writeln(js0=Js0),    
    maplist(s,Js0,Js),
    writeln(js=Js),
    append(Js,Js1),
    writeln(Js1),
    nl.

ws([],[]).
ws([W|Ws],[T-S|L]) :-
    split_string(W," ","",[T,S0]),
    number_string(S,S0),
    ws(Ws,L).

s(S,L) :- split_string(S," ","",L).