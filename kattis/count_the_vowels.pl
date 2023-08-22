% https://open.kattis.com/problems/countthevowels
% 1s
% 1.4 Easy

main :-
    read_line_to_codes(user_input,S),
    c(S,0,N),
    writeln(N).
main.
c([],N,N).
c([C|Cs],N0,N):-(v(C)->N1 is N0 +1;N1 is N0),c(Cs,N1,N).
v(C):-memberchk(C,[0'a,0'e,0'i,0'o,0'u,0'A,0'E,0'I,0'O,0'U]).
