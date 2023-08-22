% https://open.kattis.com/problems/transitwoes
% 1s
% 1.4 Easy

main :-
    rs([S,T,N]),rs(Ds0),rs(Bs),rs(Cs),
    [D|Ds] = Ds0,
    Start is S + D,
    t(Ds,Bs,Cs,Start,End),
    (End =< T -> R=yes;R=no),
    writeln(R).
main.

t([],[],[],S,S).
t([D|Ds],[B|Bs],[C|Cs],S0,S) :-
    next_modulo(S0,C,Next),
    S1 is Next + D+B,
    t(Ds,Bs,Cs,S1,S).

%
% next_modulo(N,Interval, Next)
% Next is the first time after N
% when buses arrives at intervals of Interval
%
next_modulo(N,Int, Next) :-
    I1 is Int-1,
    findall(NI,(between(0,I1,I),
               NI is N+I,
               NI mod Int =:= 0
              ),Is),
    [Next|_] = Is.

rs(Ns) :-
    read_line_to_string(user_input,S),
    split_string(S," ","", Ss),
    maplist(number_string,Ns,Ss).
