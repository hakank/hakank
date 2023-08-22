% https://open.kattis.com/problems/mia
% 1s
% 1.9 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,Ds,Ss),
    s(Ds).
s([]).
s([0,0,0,0]).
s([A1,B1,C1,D1|Ds]) :-
    c([A1,B1],[A,B]),c([C1,D1],[C,D]),g(A,B,S1), g(C,D,S2),
    (S1>S2-> T="Player 1 wins." ;(S1=:=S2->T="Tie.";T="Player 2 wins." )),
    writeln(T),
    s(Ds).

c([A,B],[A,B]) :- A >=B.
c([A,B],[B,A]) :- A < B.

g(2,1,10000).
g(X,X,V) :- V is X*X*60.
g(X,Y,V) :- X \= Y, V is X*6+Y.
