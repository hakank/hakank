% https://open.kattis.com/problems/doublepassword
% 1s
% 1.6 Easy

main :-
    read_string(user_input,1000,S),
    split_string(S,"\n","\n",Ss),
    maplist(string_chars,Ss,[A,B]),
    s(A,B,1,R),
    writeln(R).
main.

s([],[],S,S).
s([A|As],[B|Bs],S0,S) :-
    (A == B -> C = 1;C = 2),
    S1 is S0*C,
    s(As,Bs,S1,S).


%% s([],[],S,S).
%% s([A|As],[B|Bs],S0,S) :-
%%     sort([A,B],C),
%%     length(C,Len),
%%     S1 is S0*Len,
%%     s(As,Bs,S1,S).

        
    