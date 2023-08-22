% https://open.kattis.com/problems/natrij
% 1s
% 2.8 Medium

% Wrong answer on test 12/12

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n:","\n:",Ss),
    maplist(number_string,[FH,FM,FS, TH,TM,TS],Ss),
    K=60*60,
    c(FH,FM,FS,F), c(TH,TM,TS,T),
    (F > T ->
        D is (T+24*K)-F
    ;
        F =:= T ->
            D is 24*K
        ;
            D is T-F
    ),
    HH is D div K ,
    MM is (D-HH*K) div 60,
    SS is D-HH*K-MM*60,
    format('~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+~n',[HH,MM,SS]).
c(H,M,S,Ss) :- Ss is H*60*60+M*60+S.