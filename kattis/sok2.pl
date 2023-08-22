% https://open.kattis.com/problems/sok
% 1s
% 1.8 Easy

% Trying to make it shorter w/o clpr. Nope....

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[A,B,C,I,J,K],Ss),
    s([A,B,C,I,J,K],T).

s([A,B,C, I,J,K],T) :-
    Tot is I+J+K,
    p(0,A,[],As),
    p(0,B,[],Bs),
    p(0,C,[],Cs),
    findall([T,Orange,Apple,Pineapple],(member(Orange,As),member(Apple,Bs),member(Pineapple,Cs),
               T is Orange+Apple+Pineapple,                                        
               Orange =:= I*T/Tot,Apple =:= J*T/Tot,Pineapple =:= K*T/Tot
               ),Ts),
    writeln(ts=Ts),
    /*
    {Orange =< A, Apple =< B, Pineapple =< C,
     T = Orange + Apple + Pineapple,
     Orange*Tot = I*T,
     Apple*Tot = J*T,
     Pineapple*Tot = K*T
    },
    maximize(T),
    AD is (A-Orange),
    BD is (B-Apple),
    CD is (C-Pineapple),
    maplist(format('~6f '),[AD,BD,CD]),
    */
    nl.

p(I,N,S,S) :- I > N.
p(I,N,S0,[I1|S]) :-
    I1 is I+0.05,
    p(I1,N,S0,S).