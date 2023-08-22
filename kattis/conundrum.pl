% https://open.kattis.com/problems/conundrum
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    atom_chars(S,Ss),
    length(Ss,Len),
    per(Ss,['P','E','R'],0,C),
    Res is Len - C,
    writeln(Res).
main.

per([],_,S,S).
per([A,B,C|Rest],PER,S0,S) :-
    c([A,B,C],PER,0,R),
    S1 is S0 + R,
    per(Rest,PER,S1,S).

c([],_,C,C).
c([A|As],[B|Bs],C0,C) :-
    (A == B -> C1 is C0+1 ; C1 is C0 ),
    c(As,Bs,C1,C).
