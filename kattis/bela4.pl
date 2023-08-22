% https://open.kattis.com/problems/bela
% Time: 1s
% Difficulty: 1.3 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[NB|Ss]),
    split_string(NB," ","",[_,B0]),
    atom_chars(B,B0),
    findall(P,(member(X,Ss),atom_chars(X,[V,T]),v(V,D,ND),(T==B->P=D;P=ND)),Ps),
    sum_list(Ps,Sum),
    writeln(Sum).

v('A',11,11).
v('K',4,4).
v('Q',3,3).
v('J',20,2).
v('T',10,10).
v('9',14,0).
v('8',0,0).
v('7',0,0).