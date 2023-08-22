% https://open.kattis.com/problems/drunkvigenere
% 1s
% 1.6 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(string_codes,Ss,[C,K]),
    s(C,K,0,[],L),
    format("~s~n",[L]).

s([],[],_,L,L).
s([C|Cs],[K|Ks],I,L0,[T|L]) :-
    (I mod 2 =:= 0 ->X=C-K;X=C+K),
    T is 65+X mod 26,I1 is I+1,
    s(Cs,Ks,I1,L0,L).
