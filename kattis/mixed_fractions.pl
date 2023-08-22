% https://open.kattis.com/problems/mixedfractions
% 1s
% 1.7 Easy

main :-
    read_string(user_input, 10000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
main.

s([]).
s(["0 0"]).
s([S|Ss]) :-
    split_string(S," ","",Ns0),
    maplist(number_string,[A,B],Ns0),
    Div is A div B,
    Mod is A mod B,
    format('~d ~d / ~d~n',[Div,Mod,B]),
    s(Ss).
