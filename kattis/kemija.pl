% https://open.kattis.com/problems/kemija08
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S," ","\n",Ss),
    maplist(string_chars,Ss,Cs),
    s(Cs),nl.
main.

s([]).
s([W|Ws]) :-
    w(W,[],T),
    format('~s ',[T]),
    s(Ws).

w([],L,L).
w([C,'p',C|Cs],L0,L) :-
    (memberchk(C,['a','e','i','o','u']) ->
        append(L0,[C],L1),
        w(Cs,L1,L)
    ;
        append(L0,[C],L1),
        w(['p',C|Cs],L1,L)
    ).
w([C|Cs],L0,L) :-
    append(L0,[C],L1),
    w(Cs,L1,L).
