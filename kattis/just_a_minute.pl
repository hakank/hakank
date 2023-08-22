% https://open.kattis.com/problems/justaminute
% 1s
% 1.8 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns,0,Min,0,Sec),
    (Min>0->X is Sec/(Min*60),(X>1->format("~8f~n",[X]);e);e).
e:-writeln("measurement error").
s([],M,M,S,S).
s([Min,Sec|T],M0,M,S0,S) :- M1 is M0+Min,S1 is S0+Sec,s(T,M1,M,S1,S).
    