% https://open.kattis.com/problems/somesum
% 1s
% 2.0 Easy

% Picat:
% member(N,1..12),nl, foreach(C in 1..10,M=N mod 4,CC=(C..C+N-1),S=CC.sum,M2=(S mod 2)) println([n=N,m4=M,m2=M2,c=C,cc=CC,s=S]) end,fail

% N mod 4:
%  0: Even
%  1: Either
%  2: Odd
%  3: Either
%

% Same approach as in some_sum.py
% 134 chars
main :-
    read_line_to_string(user_input,S),
    number_string(N,S),E="Either",M is N mod 4,nth0(M,["Even",E,"Odd",E],T),writeln(T).


% First approach: 147 chars
main0 :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    M is N mod 4,(M=:=0->T="Even";(M=:=2->T="Odd";T="Either")),writeln(T).
        