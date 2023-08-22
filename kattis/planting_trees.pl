% https://open.kattis.com/problems/plantingtrees
% 1s
% 1.7 Easy

% It's 0.13s. I guess that sort/4 take some time for large
% instances (N <= 10**5)

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_|Ns],Ss),
    sort(0,@>=,Ns,NsS),
    s(NsS,1,0,R0),
    R is R0 + 1,
    writeln(R).

s([],_,S,S).
s([N|Ns],I,S0,S) :-
    S1 is max(N+I,S0),
    I1 is I+1,
    s(Ns,I1,S1,S).
