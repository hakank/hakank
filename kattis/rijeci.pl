% https://open.kattis.com/problems/rijeci
% 1s
% 1.7 Easy

main :-
    read_line_to_codes(user_input,S),
    number_string(N,S),
    (N =:= 1 -> writeln("0 1");
        N1 is N-1,N2 is N-2,
        findall(F,(between(1,46,I),fib(I,F)),Fs),
        nth1(N2,Fs,A),nth1(N1,Fs,B),
        format('~d ~d~n',[A,B])
    ).
:- table  fib/2.
fib(0,1):-!. fib(1,1):-!. fib(N,F):-N>1,N1 is N-1,N2 is N-2,fib(N1,F1),fib(N2,F2),F is F1+F2.


main_test :-
    read_line_to_codes(user_input,S),
    number_string(N,S),    
    s(N,[0'A],T),
    writeln(t=T),    
    msort(T,Ts),
    clumped(Ts,Cl),
    writeln(Cl),    
    member(0'A-A,Cl),
    member(0'B-B,Cl),    
    writeln([a=A,b=B]).

s(0,S,S).
s(N,S0,S) :-
    c(S0,[],S1),
    N1 is N-1,
    s(N1,S1,S).

c([],S,S).
c([0'A|Cs],S0,[0'B|S]) :-
    c(Cs,S0,S).
c([0'B|Cs],S0,[0'B,0'A|S]) :-
    c(Cs,S0,S).

