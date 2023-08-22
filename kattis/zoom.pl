% https://open.kattis.com/problems/zoom
% 1s
% 1.7 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_,KS|Ss]),
    number_string(K,KS),
    s(Ss,1,K),
    nl.
main.

s([],_,_K).
s([S|Ss],I,K) :-
    (I mod K =:= 0 ->
        format('~s ',S)
    ;
        true
    ),
    I1 is I+1,
    s(Ss,I1,K).

%% This gave Time Limit Exceeded
%% main :-
%%     read_string(user_input,100000,S),
%%     split_string(S,"\n ","\n ",[NS,KS|Ss]),
%%     maplist(number_string,[N,K],[NS,KS]),
%%     findall(_,(between(1,N,I),0 =:= I mod K,nth1(I,Ss,T),format('~s ',[T])),_),
%%     nl.
%% main.
