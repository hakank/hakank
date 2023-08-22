% https://open.kattis.com/problems/methodicmultiplication
% 2s
% 1.6 Easy

% First take:: Memory Limit Exceeded on 13/23
% The problem is in the generation of the result s/4
% which takes way too long and takes a lot of RAM for 1000*1000
%% s(0,S,S).
%% s(I,S0,S) :-
%%     writeln(i=I),
%%     atom_concat(S0,')',S1),
%%     atom_concat('S(',S1,S2),
%%     I1 is I - 1,
%%     s_x(I1,S2,S).

% OK: generating the results using findall/3 + append/3 + flatten/1 etc
% 0.32s so it's a little slower than usual...
% I guess that there's a smarter way of doing this, perhaps using DCGs
% but I don't think that's faster...
% 

%% Generating S() strings
%% test :-
%%     s(1000,'0',S),
%%     writeln(s=S),
%%     nl.

main :-
    once(read_string(user_input,10000,S)),
    split_string(S,"\n","\n",Ss),
    maplist(atom_chars,Ss,[A,B]),
    ( (A == [0] ; B == [0]) ->
        T = 0
    ;
        f(A,AN),
        f(B,BN),
        P is AN*BN,
        findall(T,(between(1,P,_),T=['S','(']),Ts1),
        findall(T,(between(1,P,_),T=[')']),Ts2),
        append(Ts1,['0'],R1),
        append(R1,Ts2,R2),
        flatten(R2,F),
        atom_chars(T,F)
    ),
    writeln(T).
main.

f(L,N) :-
    findall(C,(member(C,L),C == 'S'),Cs),
    length(Cs,N).