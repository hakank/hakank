% https://open.kattis.com/problems/basketballoneonone
% 1s
% 1.6 Easy

main :- read_line_to_codes(user_input,S),
s(S,0,A,0,B),(A > B -> writeln('A') ; writeln('B')).
s([],A,A,B,B).
s([C,N|Ss],A0,A,B0,B) :-
    (C =:= 65 -> A1 is A0 + N-48,B1 is B0;A1 is A0,B1 is B0 + N-48),
    s(Ss,A1,A,B1,B).

