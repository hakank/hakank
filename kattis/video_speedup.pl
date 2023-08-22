% https://open.kattis.com/problems/videospeedup
% 1s
% 1.8 Easy

% Nope, I don't get the correct results.
% My thought is that the first instance should be
%   ?-  X is 3*1+(10-3)*1.2+(15-10)*(1.2^2).
%    X = 18.6.
% But the correct answer should be 18.4 (not 18.6)
% What am I missing...

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_,P0,K0|Ss]),
    maplist(number_string,[P,K],[P0,K0]),
    PP is 1+(P/100),
    writeln(pp=PP),
    maplist(number_string,Ns0,Ss),
    append([Ns0,[K]],Ns),
    writeln([p=P,k=K,ss=Ss]),
    writeln(ns=Ns),
    findall(D,(nextto(A,B,Ns),D is B-A),Ds),
    length(Ds,Len),
    writeln(ds=Ds),
    findall(T,(between(1,Len,I),nth1(I,Ds,V),T is V*PP^I),Ts),
    writeln(ts=Ts),
    sumlist(Ts,R0),
    [First|_] = Ns,
    R is R0 + First,
    writeln(R).
