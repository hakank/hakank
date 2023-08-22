% https://open.kattis.com/problems/babylonian
% 1s
% 2.0 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([S|Ss]) :-
    split_string(S,",","",T),
    reverse(T,R),
    b(R,0,0,Res),
    writeln(Res),
    s(Ss).

b([],_,L,L).
b([D|Ds],I,N0,N) :-
    I1 is I+1,
    (D == "" -> b(Ds,I1,N0,N) ; number_string(V,D), N1 is N0+V*60^I, b(Ds,I1,N1,N) ).
