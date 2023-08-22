% https://open.kattis.com/problems/mjehuric
% 1s
% 1.7 Easy

main :-
    read_string(user_input,10000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,Ns,Ss),
    s(1,Ns).

s(_,S) :- sort(S,S).
s(I,Ns) :-
    (I =:= 5 -> I2 is 1, N2 = Ns
    ;
        nth1(I,Ns,A),
        I1 is I+1,
        nth1(I1,Ns,B),
        (A < B -> N2 = Ns ;
            swap(Ns,A,B,N2),
            p(N2)
        ),
        I2 = I1
    ),
    s(I2,N2).

p(L) :-
    maplist(format('~w '),L),
    nl.

swap(L,I,J,L2) :- append(A,[I,J|B],L), append(A,[J,I|B],L2).