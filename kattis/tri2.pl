% https://open.kattis.com/problems/tri
% 1s
% 1.6 Easy

% Another take, a little more elegant, and shorter

main:-read_line_to_string(user_input,S),split_string(S," ","",Ss),
maplist(number_string,[A,B,C],Ss),s(A,B,C).
main.
f(A,Op1,B,Op2,C):-
    format('~d~w~d~w~d~n',[A,Op1,B,Op2,C]).
s(A,B,C):-member(Op,[+,-,*,/]),(T=..[Op,B,C],A is T,f(A,=,B,Op,C);
T=..[Op,A,B],C is T,f(A,Op,B,=,C)).

