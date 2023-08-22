% https://open.kattis.com/problems/exam
% 1s
% 1.8 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[NS,MeS,FriendS]),
    number_string(N,NS),
    maplist(string_codes,[MeS,FriendS],[Me,Friend]),
    length(Me,Len),
    s(Me,Friend,0,NumSame),
    NumNotCorrect is Len-N,
    NumNotSame is Len-NumSame,
    X is min(N,NumSame) + min(NumNotCorrect,NumNotSame),
    writeln(X).
main.

s([],[],S,S).
s([A|As],[B|Bs],S0,S) :-
    (A =:= B ->
        S1 is S0 + 1
    ;
        S1 = S0
    ),
    s(As,Bs,S1,S).
