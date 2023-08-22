% https://open.kattis.com/problems/testdrive
% 1s
% 1.9 Easy

/*
If the car changed direction, output turned. 
Otherwise, 
  - output accelerated if it travelled a greater distance in the second minute than the first, 
  - braked if it travelled a smaller distance in the second minute than the first, or 
  - cruised if it travelled the same distance during both minutes.
*/

main :-
    read_line_to_string(user_input, S),
    split_string(S," ","", Ss),
    maplist(number_string,[A,B,C],Ss),
    t(A,B,C,X),
    writeln(X).
t(A,B,C,cruised):-B-A=:=C-B,!.
t(A,B,C,turned):-(A<B,B>C ; A>B,B<C),!.
t(A,B,C,accelerated):- ((B> A, B < C) ; (B < A, B > C)),abs(A-B) < abs(B-C).
t(_,_,_,braked).
