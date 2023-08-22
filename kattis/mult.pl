% https://open.kattis.com/problems/mult
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,[M|Ns],Ss),
    s(Ns,M).
main.

s([],_).
s([A,B|Ns],M) :-
    (A mod M =:= 0 ->
        writeln(A),
        s(Ns,B)
    ;
        s([B|Ns],M)
    ).
