% https://open.kattis.com/problems/cprnummer
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    atom_chars(S,Ss),
    delete(Ss,'-',Sss),
    maplist(atom_number,Sss,CPR),
    L = [4,3,2,7,6,5,4,3,2,1],
    scalar_product2(CPR,L,0,P),
    M is P mod 11,
    (M =:= 0 -> writeln(1) ; writeln(0)).
main.

scalar_product2([],[],Res,Res).
scalar_product2([X|Xs],[Y|Ys],Res0,Res) :-
    Res1 is Res0 + X*Y,
    scalar_product2(Xs,Ys,Res1,Res).
