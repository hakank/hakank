% https://open.kattis.com/problems/luhnchecksum
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns).
s([]).
s([N|Ns]) :-
    p(N,Cs),
    reverse(Cs,R),
    c(R,1,0,C),
    (C mod 10 =:= 0 -> writeln("PASS") ; writeln("FAIL")),
    s(Ns).

c([],_,S,S).
c([N|Ns],I,S0,S) :-
    (I mod 2 =:= 0 ->
        T is N*2,
        r(T,R),
        S1 is S0 + R
    ;
        S1 is S0 + N
    ),
    I1 is I+1,
    c(Ns,I1,S1,S).

p(N,L) :- number_codes(N,Cs), maplist(q,Cs,L).
q(C,N) :- N is C - 48.
r(N,R):- (N < 10 -> R = N; p(N,[A,B]), R is A+B).
