% https://open.kattis.com/problems/detaileddifferences
% 1s
% 1.4 Easy

% Another take using read_string/3

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_chars,Ss,Cs),
    cp(Cs).
cp([]).
cp([P1,P2|Rest]) :-
    format('~s~n~s~n',[P1,P2]),cp(P1,P2),nl,nl,cp(Rest).
cp([],[]).
cp([A|As],[B|Bs]) :- (A == B -> T = '.' ; T = '*'),write(T),cp(As,Bs).
