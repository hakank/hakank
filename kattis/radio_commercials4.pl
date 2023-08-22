% https://open.kattis.com/problems/commercials
% 1s
% 1.8 Easy

% Kadane's algo from
% https://stackoverflow.com/questions/36310568/maximum-subarray-kadanes-algorithm-tail-recursion
% here called k instead of zs_max/2-3
% It's adjusted by adding the P (cost) variable.
% And it works: 0.09s!

% Shorter without clpfd: 340 chars
% It also takes 0.09s 
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_,P|Ns],Ss),
    k(Ns,P,Max),
    writeln(Max).
k([Z|Zs], P,MSF) :-
   k(Zs, P,Z, Z, MSF).
k([],_,_, MSF, MSF).
k([Z|Zs],P, MEH0, MSF0, MSF) :-
    MEH1 is max(Z, (MEH0+Z)-P),
    MSF1 is max(MSF0, MEH1-P),
    k(Zs,P, MEH1, MSF1, MSF).


/*
% 374 chars
:- use_module(library(clpfd)).
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_,P|Ns],Ss),
    k(Ns,P,Max),
    writeln(Max).

k([Z|Zs], P,MSF) :-
   k(Zs, P,Z, Z, MSF).
k([],_,_, MSF, MSF).
k([Z|Zs],P, MEH0, MSF0, MSF) :-
    max(Z, (MEH0+Z)-P)  #= MEH1,
    max(MSF0, MEH1-P) #= MSF1,
    k(Zs,P, MEH1, MSF1, MSF).
*/