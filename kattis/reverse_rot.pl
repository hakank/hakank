% https://open.kattis.com/problems/reverserot
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s(["0"]).
s([S|Ss]) :-
    split_string(S," ","",[N0,SP]),
    number_string(N,N0),
    string_codes(SP,Cs),
    reverse(Cs,Rev),
    rot(Rev,N,Res),
    format('~s~n',[Res]),
    s(Ss).
a([65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,95,46]).
rot(Ss,N,Rs) :-
    a(Alpha),
    length(Alpha,AlphaLen),
    findall(R,(member(C,Ss),nth0(I,Alpha,C),J is (I+N) mod AlphaLen,nth0(J,Alpha,R)),Rs).


