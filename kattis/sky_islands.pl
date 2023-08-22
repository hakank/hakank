% https://open.kattis.com/problems/skyislands
% 6s (!)
% 2.0 Easy

% Time Limit Exceeded (6s) on test 7/7

:- dynamic c/2.
% :- dynamic c/2 as monotonic.
main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,_|Ns],Ss),
    (N =:= 1 ->
        T="YES"
    ;
        s(Ns),
        (t(1,N) -> T="YES" ; T="NO")
    ),
    writeln(T).

t(N,N).
t(I,N) :-
    J is I+1,
    (p(I,J) ; p(J,I)),
    t(J,N).

% :- table p/2 as monotonic.
:- table p/2.
p(A,B) :- c(A,B).
p(A,B) :- p(A,C),p(C,B).

s([]).
s([N1,N2|Ns]) :-
    assertz(c(N1,N2)),
    assertz(c(N2,N1)),
    s(Ns).