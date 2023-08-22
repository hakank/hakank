% https://open.kattis.com/problems/rijeci
% 1s
% 1.7 Easy

main:-read_line_to_codes(user_input,S),number_string(N,S),
N1 is N-1,f(N1,F1),f(N,F2),format('~d ~d~n',[F1,F2]).
:- table  f/2.
f(0,0):-!. f(1,1):-!. 
f(N,F):- N>1,N1 is N-1,N2 is N-2,f(N1,F1),f(N2,F2),F is F1+F2.
