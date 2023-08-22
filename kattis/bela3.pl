% https://open.kattis.com/problems/bela
% Time: 1s
% Difficulty: 1.3 Easy

% Even shorter, using silly variable and predicate names and idiotic layout.
% But it got from 3rd place (with about 1500 chars, includint kattio.pl, to 1st place with 431 chars.
% A few chars can still be removed...
main :- read_line_to_string(user_input,F),split_string(F," ","", [_,TS]),atom_string(T,TS),cc(T,S),writeln(S). main.
cc(T,Cs):-ra(C), cc(T,C,0,Cs).
cc(_,end_of_file,C,C).
cc(T,C,C0,Cs):-atom_chars(C,[V,S]),v(V,D,ND),(T==S->Ct=D;Ct=ND),ra(C2),C1 is C0+Ct,cc(T,C2,C1,Cs).
v('A',11,11). v('K',4,4). v('Q',3,3). v('J',20,2). v('T',10,10). v('9',14,0). v('8',0,0). v('7',0,0).
ra(A):-read_line_to_string(user_input,I), atom_string(A,I).
        