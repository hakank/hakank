% https://open.kattis.com/problems/heirsdilemma
% 1s
% 1.7 Easy

% Another take, w/o clpfd.
% The real gain was to check d/2 before all_diff/1!: 0.37s

main :-
    read_string(user_input,100000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[Low,Up],Ss),
    s(Low,Up,0,C),
    writeln(C).

s(I,Up,C,C) :- I > Up.
s(I,Up,C0,C) :-
    number_codes(I,S),
    ( (d(S,I),all_diff(S)) ->
        C1 is C0 + 1
    ;
        C1 is C0
    ),
    I1 is I+1,
    s(I1,Up,C1,C).

d([],_).
d([D|Ds],N) :-
    DD is D - 48,
    DD > 0,
    N mod DD =:= 0,
    d(Ds,N).

all_diff([]).
all_diff([H|T]) :-
    maplist(dif(H),T),
    all_diff(T).