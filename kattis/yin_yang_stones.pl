% https://open.kattis.com/problems/yinyangstones
% 1s
% 1.7 Easy

main :-
    read_line_to_codes(user_input,S),
    msort(S,Ss),clumped(Ss,[66-B,87-W]),
    (W =:= B -> T = 1 ; T = 0),writeln(T).

/*
% Compressed: 103 chars
main:-read_line_to_codes(user_input,S),msort(S,Ss),clumped(Ss,[66-B,87-W]),(W=:=B->T=1;T=0),writeln(T).

*/

/*
main :-
    read_line_to_codes(user_input,S),
    msort(S,Ss),clumped(Ss,Cl),
    writeln(cl=Cl),
    member(0'W-W,Cl),member(0'B-B,Cl),
    (W =:= B -> T = 1 ; T = 0),writeln(T).
*/