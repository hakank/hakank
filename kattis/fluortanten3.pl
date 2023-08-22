% https://open.kattis.com/problems/fluortanten
% Time limit: 3s
% Difficulty: 1.2-2.4 Easy

% Another approach. Much faster: 0.99s!

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ", "\n ", [_|Ns0]),
    maplist(number_string,Ns,Ns0),
    select(0,Ns,Ns2), 
    pre(Ns2,1,Pre0),
    append([0],Pre0,Pre),
    Ns3 = [0|Ns2],
    isum(Ns3,1,0,Tot),
    Asum = 0,
    happiness(Ns3,Pre,1,Asum,Tot,Tot,Max),
    writeln(Max).
main.

pre([],_,[]).
pre([N|Ns],I,[T|P]) :- T is N*I,I1 is I+1, pre(Ns,I1,P).

happiness([],_Pre,_I,_Asum,_RestSum,M,M).
happiness([N|Ns],[P|Ps],I,Asum0,RestSum0,M0,M) :-
    RestSum1 is RestSum0 - N*I,
    Asum1 is Asum0 + P,
    M1 is max(M0,Asum1+RestSum1),
    I1 is I+1,    
    happiness(Ns,Ps,I1,Asum1,RestSum1,M1,M).

isum([],_,S,S).
isum([N|Ns],I,S0,S) :-S1 is S0+N*I, I1 is I+1,isum(Ns,I1,S1,S).

