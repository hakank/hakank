% https://open.kattis.com/problems/codetosavelives
% 1s
% 1.6 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n", Ss),
    maplist(number_string,[_|Ns],Ss),
    s(Ns).
main.

s([]).
s([A,B|Ns]) :-
    C is A+B,
    number_chars(C,Cs),
    maplist(format('~w '),Cs),nl,
    s(Ns).