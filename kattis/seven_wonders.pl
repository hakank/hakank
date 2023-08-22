% https://open.kattis.com/problems/sevenwonders
% 1s
% 1.5 Easy

% First submission: 610 chars, vs Andriy Zyevakov's 277 chars

main :-
    read_line_to_string(user_input,S),
    string_chars(S,Ss),
    msort(Ss,Sorted),
    clumped(Sorted,Clumped),
    length(Clumped,Len),
    findall(C,member(_-C,Clumped),All),
    findall(T,(member(C,All),T is C*C),Ts),    
    sum_list(Ts,Sum),
    (Len == 3 ->
        c(All,0,Complete),
        Tot is Sum + Complete
    ;
        Tot is Sum
    ),
    writeln(Tot).
main.

c([C,G,T],S,S) :- C =:= 0; G =:= 0 ; T =:= 0.
c([C,G,T],S0,S) :-
    ( (C > 0, G > 0, T > 0) ->
        C1 is C-1, G1 is G-1, T1 is T-1,
        S1 is S0 + 7,
        c([C1,G1,T1],S1,S)
    ;
        c([C,G,T],S0,S)
    ).