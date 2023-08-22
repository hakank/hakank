% https://open.kattis.com/problems/racingalphabet
% 1s
% 1.6 Easy

% TODO:
% Hmm, I miss something here in the calculation of the total
% time which should be floats and larger...


main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(string_codes,Ss,Cs),
    writeln(cs=Cs),
    s(Cs).
main.

d(0' ,25) :- !.
d(0'',26) :- !.
d(X, Y) :- Y is X-65.

s([]).
s([L|Ls]) :-
    writeln(l=L),
    maplist(d,L,L2),
    writeln(l2=L2),
    t(L2,0,T),
    writeln(t=T),
    length(L,Len),
    writeln(len=Len),
    T2 is T*(Len) / 15,
    writeln(t2=T2),
    s(Ls).

t([],S,S).
t([_],S,S).
t([A,B|L],S0,S) :-
    S1 is S0 + abs(A-B),
    t(L,S1,S).


/*
main :-
    rs(_),
    ra(L),
    s(L).
main.

d(0' ,25) :- !.
d(0'',26) :- !.
d(X, Y) :- Y is X-65.

s([]).
s([L|Ls]) :-
    writeln(l=L),
    maplist(d,L,L2),
    writeln(l2=L2),
    t(L2,0,T),
    writeln(t=T),
    length(L,Len),
    writeln(len=Len),
    T2 is T*(Len) / 15,
    writeln(t2=T2),
    s(Ls).

t([],S,S).
t([_],S,S).
t([A,B|L],S0,S) :-
    S1 is S0 + abs(A-B),
    t(L,S1,S).

ra(S):-rs(In),ra(In,[],S).
ra(end_of_file,S,S).
ra(In,S0,[In|S]):-rs(S2),ra(S2,S0,S).
rs(S):-read_line_to_codes(user_input,S).
*/

