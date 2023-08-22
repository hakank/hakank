% https://open.kattis.com/problems/tri
% 1s
% 1.6 Easy

main:-read_line_to_string(user_input,S),split_string(S," ","",Ss),
maplist(number_string,[A,B,C],Ss),s(A,B,C).
main.
f(A,Op1,B,Op2,C):-format('~d~w~d~w~d~n',[A,Op1,B,Op2,C]).
s(A,B,C):-(A is B+C,f(A,=,B,+,C);A is B-C,f(A,=,B,-,C);A is B*C,f(A,=,B,*,C);A is B/C,f(A,=,B,/,C)).
s(A,B,C):-(C is A+B,f(A,+,B,=,C);C is A-B,f(A,-,B,=,C);C is A*B,f(A,*,B,=,C);C is A/B,f(A,/,B,=,C)).        
